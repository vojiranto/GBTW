{-#Language DuplicateRecordFields#-}
module Data where

import qualified    Data.Text                           as T

data PNode
    = PNodeMolecule     Molecule
    | PNodeReaction     Reaction
    | PNodeREAGNT_IN    REAGNT_IN
    | PNodePRODUCT_FROM PRODUCT_FROM
  deriving Show


data Molecule       = Molecule {
    id_         :: Int,
    smiles      :: T.Text,
    iupacName   :: T.Text
  } deriving Show


data Reaction       = Reaction {
    id_         :: Int,
    name        :: T.Text,
    reagents    :: [(Molecule, REAGNT_IN)],
    catalysts   :: [(Catalyst, ACCELERATE)],
    products    :: [(Molecule, PRODUCT_FROM)]
  } 
  | ReactionSimple {
    id_         :: Int,
    name        :: T.Text
  } deriving Show


data Catalyst       = Catalyst {
    id_         :: Int,
    smiles      :: T.Text,
    name        :: Maybe T.Text
  } deriving Show


data REAGNT_IN      = REAGNT_IN
  deriving Show


data ACCELERATE   = ACCELERATE {
    temperature :: Double,
    pressure    :: Double
  } deriving Show


data PRODUCT_FROM = PRODUCT_FROM {
    amount :: Double
  } deriving Show


type Path = [PNode]
