module Program where

import Prelude
import Control.Monad.Writer.Trans (WriterT, runWriterT, tell)
import Data.Tuple (Tuple(Tuple))
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Freer.Free
  ( Constructors
  , constructors
  , interpreter
  )

import Effect (Effect)
import Node.Buffer (toString)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Show.Generic (genericShow)
import Node.ChildProcess (defaultExecSyncOptions, execSync)
import Node.Encoding (Encoding(UTF8))
import Control.Monad.Trans.Class (lift)

data ProgramF a
  = Command String (String → a)

derive instance Functor ProgramF
derive instance Generic (ProgramF a) _

type Program = Free ProgramF

newtype Step = Step { input ∷ String, output ∷ String }

derive instance Generic Step _

instance Show Step where
  show = genericShow

interpret ∷ ProgramF ~> WriterT (List Step) Effect
interpret = interpreter { command }

command ∷ String → WriterT (List Step) Effect String
command input = do
  outputBuffer ← lift $ execSync input defaultExecSyncOptions
  output ← lift $ toString UTF8 outputBuffer
  tell $ List.fromFoldable [ Step { input, output } ]
  pure output

f ∷ Constructors ProgramF Program
f = constructors (liftF ∷ ProgramF ~> Program)

run ∷ Program Unit → Effect (List Step)
run program = do
  Tuple _ steps ← runWriterT $ foldFree interpret program
  pure steps
