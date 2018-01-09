{-# Language 
    DuplicateRecordFields,
    OverloadedStrings
#-}
import              Lib
import              Data
import              Data.Monoid
import qualified    Data.Text       as T
import              Control.Monad
import              Database.Neo4j
import              Data.Aeson

molecule' :: Int -> Molecule
molecule' i          = Molecule {
    id_             = i,
    smiles          = "Molecule" <> (T.pack.show) i,
    iupacName       = "Molecule" <> (T.pack.show) i
  }


reagent' :: Int -> (Molecule, REAGNT_IN)
reagent' i           = (molecule' i, REAGNT_IN)


catalyst' :: Int -> (Catalyst, ACCELERATE)
catalyst' i          = (
    Catalyst {
        id_         = i,
        smiles      = "Catalyst" <> (T.pack.show) i,
        name        = Just $ "Catalyst" <> (T.pack.show) i
    },
    ACCELERATE {
        temperature = 120,
        pressure    = 120
    })


product' :: Int -> (Molecule, PRODUCT_FROM)
product' i = (
    molecule' i,
        PRODUCT_FROM {
            amount   = 120
        }
    )


main :: IO ()
main = do
    void $ withAuthConnection "0.0.0.0" 7474 ("neo4j", "neo4j") $ do
        -- наполняем БД
        insertReactionInDB $ Reaction {
            id_         = 1,
            name        = "reaction1",
            reagents    = [reagent' 44],
            catalysts   = [catalyst' 21],
            products    = [product' 45]
        }
        
        insertReactionInDB $ Reaction {
            id_         = 2,
            name        = "reaction2",
            reagents    = [reagent' 45, reagent' 46, reagent' 47],
            catalysts   = [catalyst' 22],
            products    = [product' 48]
        }
        
        insertReactionInDB $ Reaction {
            id_         = 3,
            name        = "reaction3",
            reagents    = [reagent' 48],
            catalysts   = [catalyst' 23],
            products    = [product' 49]
        }

        insertReactionInDB $ Reaction {
            id_         = 4,
            name        = "reaction4",
            reagents    = [reagent' 50],
            catalysts   = [catalyst' 24, catalyst' 25],
            products    = [product' 51]
        }

        insertReactionInDB $ Reaction {
            id_         = 5,
            name        = "reaction5",
            reagents    = [reagent' 52],
            catalysts   = [catalyst' 22],
            products    = [product' 53]
        }
        
        insertReactionInDB $ Reaction {
            id_         = 6,
            name        = "reaction6",
            reagents    = [reagent' 52],
            catalysts   = [catalyst' 26],
            products    = [product' 54]
        }
        
        insertReactionInDB $ Reaction {
            id_         = 7,
            name        = "reaction7",
            reagents    = [reagent' 54, reagent' 55, reagent' 56, reagent' 44],
            catalysts   = [catalyst' 27, catalyst' 28, catalyst' 21],
            products    = [product' 57, product' 49]
        }
        
        insertReactionInDB $ Reaction {
            id_         = 8,
            name        = "reaction8",
            reagents    = [reagent' 58],
            catalysts   = [catalyst' 29],
            products    = [product' 59, product' 60]
        }

        insertReactionInDB $ Reaction {
            id_         = 9,
            name        = "reaction9",
            reagents    = [reagent' 61],
            catalysts   = [catalyst' 30],
            products    = [product' 62]
        }

        insertReactionInDB $ Reaction {
            id_         = 10,
            name        = "reaction10",
            reagents    = [reagent' 60],
            catalysts   = [catalyst' 31],
            products    = [product' 63, product' 64, product' 65]
        }
        
        insertReactionInDB $ Reaction {
            id_         = 11,
            name        = "reaction11",
            reagents    = [reagent' 66],
            catalysts   = [catalyst' 32],
            products    = [product' 67]
        }
        
        insertReactionInDB $ Reaction {
            id_         = 12,
            name        = "reaction12",
            reagents    = [reagent' 67, reagent' 68, reagent' 69],
            catalysts   = [catalyst' 33, catalyst' 34, catalyst' 27],
            products    = [product' 70]
        }
        
        insertReactionInDB $ Reaction {
            id_         = 13,
            name        = "reaction13",
            reagents    = [reagent' 68],
            catalysts   = [catalyst' 35],
            products    = [product' 71]
        }

        insertReactionInDB $ Reaction {
            id_         = 14,
            name        = "reaction14",
            reagents    = [reagent' 68],
            catalysts   = [catalyst' 36],
            products    = [product' 72]
        }

        insertReactionInDB $ Reaction {
            id_         = 15,
            name        = "reaction15",
            reagents    = [reagent' 72],
            catalysts   = [catalyst' 37],
            products    = [product' 73]
        }
        
        insertReactionInDB $ Reaction {
            id_         = 16,
            name        = "reaction16",
            reagents    = [reagent' 74],
            catalysts   = [catalyst' 38, catalyst' 39, catalyst' 31],
            products    = [product' 75]
        }
        
        insertReactionInDB $ Reaction {
            id_         = 17,
            name        = "reaction17",
            reagents    = [reagent' 76, reagent' 77],
            catalysts   = [catalyst' 40],
            products    = [product' 78]
        }
        
        insertReactionInDB $ Reaction {
            id_         = 18,
            name        = "reaction18",
            reagents    = [reagent' 77],
            catalysts   = [],
            products    = [product' 79]
        }

        insertReactionInDB $ Reaction {
            id_         = 19,
            name        = "reaction19",
            reagents    = [reagent' 79],
            catalysts   = [catalyst' 41],
            products    = [product' 80]
        }

        insertReactionInDB $ Reaction {
            id_         = 20,
            name        = "reaction20",
            reagents    = [reagent' 80],
            catalysts   = [catalyst' 42, catalyst' 43],
            products    = [product' 81]
        }

    
    result <- withAuthConnection "0.0.0.0" 7474 ("neo4j", "neo4j") $ do
        getPath (molecule' 44) (molecule' 49)
    -- проверяем, что был найден и что его длина равна длине кратчайшего
    putStrLn "\n"
    case result of
        Right res | length res == 5 -> putStrLn "getPath ok"
        _                           -> putStrLn "getPath error"
    
    result <- withAuthConnection "0.0.0.0" 7474 ("neo4j", "neo4j") $ do
        getReaction 11
    case result of
        Right (Success res) | id_ (res :: Reaction) == 11 -> putStrLn "getReaction ok"
        _                                                 -> putStrLn "getReaction error"

