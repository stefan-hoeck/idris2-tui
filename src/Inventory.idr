||| Tracks the user's groceries.
|||
||| The inventory is stored as directory of JSON-encoded files, one
||| for each pantry item.
|||
||| The inventory tracks food at the granularity of individual
||| containers of food.
module Inventory


import Data.List1
import Data.Vect
import Data.SortedMap
import JSON.Derive
import Data.String
import System
import System.File
import System.Directory
import System.Concurrency


import Barcode
import Control.ANSI
import Date
import Food
import Measures
import USBScale


%default total
%language ElabReflection


||| The type of container IDs.
|||
||| For now these are just arbitrary string keys.
0 Id : Type
Id = String


||| How long a given food item is expected to last.
|||
||| This is meant to capture the different representations of product
||| life which are used on packaged and unpackaged foods. From this we
||| can calculate when action must be taken for a given food, and
||| prompt the user to take appropriate actions.
data LifeTime
  = BestBy        Date
  | UseBy         Date
  | UseOrFreezeBy Date
  | Expires       Date
  | Forever
  | Unknown
%runElab derive "LifeTime" [Show,Eq,FromJSON,ToJSON]

||| Conservatively report the expiry date of a container
expiryDate : LifeTime -> Maybe Date
expiryDate (BestBy x)        = Just $ x
expiryDate (UseBy x)         = Just $ x
expiryDate (UseOrFreezeBy x) = Just $ x
expiryDate (Expires x)       = Just $ x
expiryDate Forever           = Nothing
expiryDate Unknown           = Nothing


||| True if the container expires on or after the given date
expiresOn : Date -> LifeTime -> Bool
expiresOn date lt = case expiryDate lt of
  Nothing => False
  Just e  => date <= e


||| The type of container.
data ContainerType
  = WholeFood
  | Sealed
  | Opened
  | Reusable
%runElab derive "ContainerType" [Show,Eq,FromJSON,ToJSON]

||| A single container of a given food.
|||
||| Each container has a unique numeric ID. The ID is arbitrarily
||| chosen by the user.
|||
||| IDs may be generated by the user interface, scanned from
||| pre-printed labels, or hand-chosen by the user.
|||
||| For each container, we track its lifetime, empty weight, and
||| current weight.
record Container where
  constructor MkContainer
  food:    Barcode
  life:    LifeTime
  type:    ContainerType
  empty:   Weight
  current: Weight
%runElab derive "Container" [Show,Eq,FromJSON,ToJSON]

||| The different ways we can query the inventory.
data Query
  = ByBarcode     Barcode
  | ExpiresOn     Date
  | ByFoodName    String
  | And           Query Query
  | All
%runElab derive "Query" [Show, Eq]
||| An Inventory is a set of containers indexed by their Id
0 Inventory : Type
Inventory = SortedMap Id Container

filterInv : (Container -> Bool) -> Inventory -> Inventory
filterInv f inv = fromList $ filter (f . snd) $ toList inv

query : Query -> Container -> Bool
query (ByBarcode x)    c = c.food == x
query (ExpiresOn x)    c = expiresOn x c.life
query (ByFoodName str) c = ?hole
query (And a b)        c = (query a c) && (query b c)
query All              _ = True

{- Command line parsing ****************************************** -}


||| How to get user input for a value.
|||
||| Not all modes of input make sense for all types, so this is
||| indexed over the type of the data, with specific constructors for
||| each type.
data Prompt : a -> Type where
  Direct     : a     -> Prompt a
  FromScale  :          Prompt Weight
  QueryFood  : Query -> Prompt Barcode
  ChooseFood :          Prompt Barcode
  QueryId    : Query -> Prompt Id
  ChooseId   :          Prompt Id

Show a => Show (Prompt a) where
  show (Direct x)    = "(Direct \{show x})"
  show FromScale     = "FromScale"
  show (QueryFood x) = "(QueryFood \{show x})"
  show ChooseFood    = "ChooseFood"
  show (QueryId x)   = "(QueryId \{show x})"
  show ChooseId      = "ChooseId"


||| The high level commands on the inventory.
|||
||| Where the user has a choice of input methods, we wrap the required
||| type in "Prompt".
data Command
  = Search   Query
  | Show     (Prompt Id)
  | Weigh    (Prompt Id) (Prompt Weight)
  | Create   (Prompt Id)
             (Prompt Barcode)
             LifeTime
             ContainerType
             (Prompt Weight)
             (Prompt Weight)
  | Transfer (Prompt Id) (Prompt Id)
  | Delete   (Prompt Id)
%runElab derive "Command" [Show]


||| Parse a single term in a query.
|||
||| A term is either a bare word, or prefixed with an operator, as is
||| done in email clients and issue trackers.
parseQueryTerm : String -> Maybe Query
parseQueryTerm term = case toList $ split (== ':') term of
  ["all"]        => Just All
  [term]         => Just $ ByFoodName term
  ["name",    n] => Just $ ByFoodName n
  ["barcode", b] => map ByBarcode  $ fromDigits b
  ["expires", d] => map ExpiresOn  $ fromString d
  _              => Nothing

||| Recursively parse an entire query
|||
||| Juxtaposed terms are implicitly combined via `And`
parseQuery : List String -> Maybe Query
parseQuery [] = Nothing
parseQuery [term] = parseQueryTerm term
parseQuery (term  :: rest)  = Just $ And !(parseQueryTerm term) !(parseQuery rest)

||| XXX: this can't handle the number 0, so is broken.
parseNat : String -> Maybe Nat
parseNat s = case stringToNatOrZ s of
  Z => Nothing
  x => Just x

||| Parse an ID in a command
|||
||| The special value `choose` will prompt the user to choose from a list.
parseId : String -> Maybe $ Prompt Id
parseId "choose" = Just ChooseId
parseId id       = Just $ Direct id

||| Parse a barcode in a command
|||
||| The special value `choose` will prompt the user to choose from a list.
parseBarcode  : String      -> Maybe $ Prompt Barcode
parseBarcode "choose" = Just ChooseFood
parseBarcode bc = map Direct $ fromDigits bc

||| Parse a container lifetime
|||
||| This is either one of the special keywords `forever`, `unknown`,
||| or a prefixed date or day value.
parseLifeTime : String -> Maybe LifeTime
parseLifeTime "forever" = Just Forever
parseLifeTime "unknown" = Just Unknown
parseLifeTime lifetime =
  let
    parts := toList $ split (== ':') lifetime
  in case parts of
    ["best-by", date]          => map BestBy        $ fromString date
    ["use-by",  date]          => map UseBy         $ fromString date
    ["use-or-freeze-by", date] => map UseOrFreezeBy $ fromString date
    ["expires", date]          => map Expires       $ fromString date
    _                          => Nothing

||| Parse container type in a command line
parseContainerType: String  -> Maybe ContainerType
parseContainerType "whole"    = Just WholeFood
parseContainerType "opened"   = Just Opened
parseContainerType "sealed"   = Just Sealed
parseContainerType "resuable" = Just Reusable
parseContainerType _          = Nothing

||| Parse a mass value in a command line.
parseWeight : String -> Maybe $ Prompt Weight
parseWeight "scale" = Just $ FromScale
parseWeight str = map Direct $ quantityFromString str

||| Collect the complete inventory record from the user.
parseCreateCmd : List String -> Maybe Command
parseCreateCmd [id, bc, lt, ct, ew, cw] = do
  id <- parseId id
  bc <- parseBarcode bc
  lt <- parseLifeTime lt
  ct <- parseContainerType ct
  ew <- parseWeight ew
  cw <- parseWeight cw
  pure $ Create id bc lt ct ew cw
parseCreateCmd _ = Nothing

||| Parse the entire command line
parse : List String -> Maybe Command
parse []                     = Nothing
parse ("search" :: query)    = map Search $ parseQuery query
parse ["show",   id]         = map Show   $ parseId id
parse ["weigh",  id, weight] = Just $ Weigh !(parseId id) !(parseWeight weight)
parse ("create" :: rest)     = parseCreateCmd rest
parse ["transfer", a, b]     = Just $ Transfer !(parseId a) !(parseId b)
parse ["delete", id]         = map Delete $ parseId id
parse _                      = Nothing


{- IO ------------------------------------------------------------------------}


idToPath : String -> String
idToPath id = "inventory/\{id}.json"

||| Read a single container entry from the inventory directory
covering
readContainer : String -> IO (Either String Container)
readContainer file = do
  Right contents <- readFile file | Left _ => pure $ Left "Invalid File Name"
  case decode contents of
    Right c => pure $ Right c
    Left  e => pure $ Left "Corrupt file \{file}: \{show e}"

||| Read the given list of containers from the inventory directory
covering
readContainers : List String -> IO (Either String Inventory)
readContainers [] = pure $ Right empty
readContainers (file :: files) = do
  Right cur <- readContainer file
              | Left _ => pure $ Left "Couldn't read file for item \{file}"
  Right rest <- readContainers files
              | Left _ => pure $ Left "Couldn't read rest of the files"
  pure $ Right $ insert file cur rest


||| Open the inventory database at the given path
covering
readInventory   : String -> IO (Either String Inventory)
readInventory path = do
  Right files <- listDir path | Left _ => pure $ Left "Invalid Path"
  readContainers $ map ("inventory/" ++) files

||| Save the given record to the inventory
covering
saveContainer : Id -> Container -> IO Builtin.Unit
saveContainer file c = do
  Right _ <- writeFile file (encode c) | Left err => putStrLn (show err)
  putStrLn "Saved: \{file}"

||| Run the function on the given inventory
covering
withInventory : String -> (Inventory -> Inventory) -> IO (Either String Inventory)
withInventory path f = pure $ map f !(readInventory path)


{- Command Processing --------------------------------------------------------}

covering
runPrompt : String -> Prompt x -> IO x
runPrompt _ (Direct y) = pure y
runPrompt device FromScale = case !(getWeight device) of
  Left  err => die "couldn't read scale"
  Right weight => pure weight
runPrompt _ (QueryFood y) = ?runPrompt_rhs_2
runPrompt _ ChooseFood = ?runPrompt_rhs_3
runPrompt _ (QueryId y) = ?runPrompt_rhs_4
runPrompt _ ChooseId = ?runPrompt_rhs_5

covering
run : String -> Command -> IO Builtin.Unit
run path (Search x)  = do
  Right result <- withInventory "inventory" (filterInv (query x))
                | Left err => putStrLn "\{show err}"
  for_ (SortedMap.toList result) $ \(id,c) => putStrLn "\{id}: \{show $ c}"
run path (Show x)    = do
  id <- runPrompt path x
  let file = idToPath id
  Right cont <- readContainer file | Left err => putStrLn "\{show err}: \{id}"
  putStrLn $ show cont
run path (Weigh id w) = do
  id <- runPrompt path id
  let file = idToPath id
  Right cont <- readContainer file | Left err => putStrLn "\{show err}: \{id}"
  cw <- runPrompt path w
  saveContainer file $ {current := cw} cont
run path (Create id bc lt ct ew cw) = do
  id <- runPrompt path id
  bc <- runPrompt path bc
  ew <- runPrompt path ew
  cw <- runPrompt path cw
  let file = idToPath id
  saveContainer file (MkContainer bc lt ct ew cw)
run _ (Transfer x y) = ?hole_4
run path (Delete id) = do
  id <- runPrompt path id
  let file = idToPath id
  Right _ <- removeFile file | Left err => putStrLn $ show err
  putStrLn "Deleted: \{id}"


{- Entry Point ---------------------------------------------------------------}


||| Entry point for the `inventory` subcommand.
partial export
main : List String -> IO Builtin.Unit
main args = do
  Just scale_device_path <- getEnv "AMPII_SCALE_DEVICE"
                         | Nothing => putStrLn "No scale device"
  case parse args of
    Nothing => putStrLn "Invalid command: \{unwords args}"
    Just cmd => do
      putStrLn $ show cmd
      run scale_device_path cmd
