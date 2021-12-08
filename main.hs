import           Control.Monad         (void)
import qualified Data.ByteString.Char8 as C
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (catMaybes)
import           Ledger                (Address, Datum (Datum), ScriptContext, Validator, Value)
import qualified Ledger
import qualified Ledger.Ada            as Ada
import qualified Ledger.Constraints    as Constraints
import           Ledger.Tx             (ChainIndexTxOut (..))
import qualified Ledger.Typed.Scripts  as Scripts
import           Playground.Contract
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import qualified Prelude               as Haskell

validateGuess :: Bool -> Bool -> ScriptContext -> Bool
validateGuess actual guess _ = traceIfFalse "wrong guess" $ actual == guess

data LockParams = LockParams
    { 
        amount :: Value,
        bet :: Integer
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data GuessParams = GuessParams
    { evenOrOdd :: Bool }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

type GameSchema =
    Endpoint "lock" LockParams
    .\/ Endpoint "guess" GuessParams

data Game
instance Scripts.ValidatorTypes Game where
    type instance RedeemerType Game = Bool
    type instance DatumType Game = Bool

gameInstance :: Scripts.TypedValidator Game
gameInstance = Scripts.mkTypedValidator @Game
    $$(PlutusTx.compile [|| validateGuess ||])
    $$(PlutusTx.compile [|| Scripts.wrapValidator @Bool @Bool ||])

lock :: AsContractError e => Promise () GameSchema e ()
lock = endpoint @"lock" @LockParams $ \(LockParams amt betNumber) -> do
    let isEven = Haskell.even betNumber
        tx     = Constraints.mustPayToTheScript isEven amt
    void $ submitTxConstraints gameInstance tx

guess :: AsContractError e => Promise () GameSchema e ()
guess = endpoint @"guess" @GuessParams $ \(GuessParams guessed) -> do
    unspentOutputs <- utxosAt gameAddress
    let tx = collectFromScript unspentOutputs guessed
    void $ submitTxConstraintsSpending gameInstance unspentOutputs tx

-- | The validator script of the game.
gameValidator :: Validator
gameValidator = Scripts.validatorScript gameInstance

game :: AsContractError e => Contract () GameSchema e ()
game = do
    logInfo @Haskell.String "Waiting for guess or lock endpoint..."
    selectList [lock, guess]

gameAddress :: Address
gameAddress = Ledger.scriptAddress gameValidator

endpoints :: AsContractError e => Contract () GameSchema e ()
endpoints = game

mkSchemaDefinitions ''GameSchema

$(mkKnownCurrencies [])