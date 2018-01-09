{-# Language 
    OverloadedStrings,
    OverloadedLists,
    DuplicateRecordFields,
    MultiWayIf
#-}
module Lib (
    someFunc,
    getPath,
    insertReactionInDB,
    getReaction
  ) where

import              Data
import qualified    Data.HashMap.Lazy                   as M
import qualified    Data.Text                           as T
import              Data.Int
import              Data.Aeson.Types
import qualified    Database.Neo4j.Batch                as B
import              Data.Monoid
import              Data.Maybe
import qualified    Database.Neo4j.Transactional.Cypher as TC
import qualified    Database.Neo4j.Graph                as G
import              Database.Neo4j
import              Control.Monad
import              Data.Scientific
import              Data.List
import              Data.Function
import qualified    Data.Vector                         as V


someFunc :: IO ()
someFunc = do
    putStrLn "Примеры использования в тестах"

-- | Добавляем реакцию в базу
insertReactionInDB :: Reaction -> Neo4j ()
insertReactionInDB r = do
    rNode <- createNode [
        "name"  |: (name (r :: Reaction) :: T.Text),
        "id"    |: (toEnum (id_  (r :: Reaction)) :: Int64)
      ]
    addLabels ["Reaction"] rNode
    let createTheRelationTo typeName node prop = void $
            createRelationship typeName prop rNode node
        createTheRelationFrom typeName node prop = void $
            createRelationship typeName prop node rNode
    forM_ (reagents r) (create createTheRelationFrom)
    forM_ (catalysts r) (create createTheRelationFrom)
    forM_ (products r) (create createTheRelationTo)


-- | Извлекаем ноду по свойству
getByProp :: T.Text -> T.Text -> T.Text -> Neo4j [Node]
getByProp typeName prop propName = do 
    res <- TC.runTransaction $ TC.cypher
        ("MATCH (m:" <> typeName <> " {" <> propName <> ":'" <> prop <>"'})" <>
         "RETURN m") M.empty
    resultToListForm res


-- | Приводим результат запроса в форму списка. 
resultToListForm :: Monad m => Either a0 TC.Result -> m [Node]
resultToListForm res = case res of
    Left _     -> return []
    Right rRes -> return $ concatMap G.getNodes $ TC.graph rRes


-- | Создаём связь с нодой, если таковая есть, или создаём ещё и ноду
create :: (GetSmile a1, Createble a1, Createble a2) => 
    (T.Text -> Node -> M.HashMap T.Text PropertyValue -> Neo4j b) -> (a1, a2) -> Neo4j b
create func (p, rel) = do
    mNode <- getByProp (typeIs p) (getSmiles p) ("smiles")
    if length mNode > 0 then func (typeIs rel) (head mNode) (params rel)
    else do
        node <- (createNode.params) p
        addLabels [typeIs p] node
        func (typeIs rel) node (params rel)


class GetSmile a where
    getSmiles :: a -> T.Text
instance GetSmile Molecule where
    getSmiles a = smiles (a :: Molecule)
instance GetSmile Catalyst where
    getSmiles a = smiles (a :: Catalyst)


-- | Объекты могущие быть сохранены в БД
class Createble a where
    typeIs :: a -> T.Text
    params :: a -> M.HashMap T.Text PropertyValue


instance Createble REAGNT_IN where
    typeIs _    = "REAGNT_IN"
    params _    = M.empty


instance Createble ACCELERATE where
    typeIs _    = "ACCELERATE"
    params rel  = [
        "temperature"   |: temperature rel,
        "pressure"      |: temperature rel
      ]


instance Createble PRODUCT_FROM where
    typeIs _    = "PRODUCT_FROM"
    params rel  = ["amount" |: amount rel]


instance Createble Molecule where
    typeIs _ = "Molecule"
    params m = [
        "id"        |: (toEnum (id_ (m :: Molecule)) :: Int64),
        "smiles"    |: (smiles      (m :: Molecule)  :: T.Text),
        "iupacName" |: (iupacName   (m :: Molecule)  :: T.Text)
      ]


instance Createble Catalyst where
    typeIs _        = "Catalyst"
    params m        = M.fromList $
        ("smiles"    |: (smiles (m :: Catalyst) :: T.Text)):
        ("id"        |: (toEnum (id_ (m :: Catalyst)) :: Int64)):nodeName
      where
        name'       = name (m :: Catalyst)
        nodeName    = if isJust name'
            then ["name"   |: (fromJust name' :: T.Text)]
            else []


toMolecule :: Value -> Parser Molecule
toMolecule (Object o)   = Molecule
    <$> o .: "id"
    <*> o .: "smiles" 
    <*> o .: "iupacName"
toMolecule _            = mzero


toCatalyst :: Value -> Parser Catalyst
toCatalyst (Object o)   = Catalyst
    <$> o .:  "id"
    <*> o .:  "smiles"
    <*> o .:? "name"   
toCatalyst     _        = mzero 


toAcelerate :: Value -> Parser ACCELERATE
toAcelerate (Object o)  = ACCELERATE 
    <$> o .: "temperature"
    <*> o .: "pressure"
toAcelerate _           = mzero


toProduct :: Value -> Parser PRODUCT_FROM
toProduct (Object o)    = PRODUCT_FROM
    <$> o .: "amount"
toProduct _             = mzero


toReaction :: Value -> Parser Reaction
toReaction (Object o)   = ReactionSimple
    <$> o .: "id"
    <*> o .: "name"


-- | По номеру реакции в базе возвращаtn её haskell-объект;
getReaction :: Int -> Neo4j (Either TC.TransError (Result Reaction))
getReaction i = TC.runTransaction $ do
    let theReation = "MATCH (a:Reaction {id : " <> (T.pack.show) i <> "}) "
        theRelation a = "-[b:" <> a <> "]-(c)\n" 
    reac       <- TC.cypher (theReation <>                               "RETURN a")    M.empty
    reagents'  <- TC.cypher (theReation <> theRelation "REAGNT_IN"    <> "RETURN c, b") M.empty
    catalysts' <- TC.cypher (theReation <> theRelation "ACCELERATE"   <> "RETURN c, b") M.empty
    products'  <- TC.cypher (theReation <> theRelation "PRODUCT_FROM" <> "RETURN c, b") M.empty
    
    let forM' xs fx fy = forM 
            (spliter (head $ TC.vals xs) (parse fx) (parse fy))
            (\(a, b) -> (,) <$> a <*> b)
    pure $ Reaction
        <$> pure i
        <*> (parse (\(Object a) -> a .: "name") (head $ head $ TC.vals reac))
        <*> (forM' reagents'  toMolecule (const $ pure REAGNT_IN))
        <*> (forM' catalysts' toCatalyst toAcelerate)
        <*> (forM' products'  toMolecule toProduct)


-- | Извлекаем пары нод и ведущих к ним рёбер
spliter (x:y:xs) fx fy  = (fx x, fy y):spliter xs fx fy
spliter _ _ _           = []


-- | Ищим кратчайший путь между молекулами
getPath a b = do
    TC.runTransaction $ do
        result <- TC.cypher 
            ("MATCH p = (a:Molecule {smiles:'" <> smiles (a :: Molecule) <>
                 "'})-[*]->(b:Molecule {smiles: '" <> smiles (b :: Molecule) <>
                 "'})\n" <>
            " RETURN p")
            M.empty
            
        let arrays :: [Value]
            arrays = concat $ TC.vals result
            array :: [Value]
            array  = if arrays /= [] 
                then V.toList $ minimumBy (compare`on`V.length) ((\(Array a) -> a) <$> arrays)
                else []
        path <- forM (parsePath array) $ \i -> pure i
        let isSuccess   (Success _) = True
            fromSuccess (Success a) = a
        pure $ if all isSuccess path then fromSuccess <$> path else []


-- | Преобразуем путь в удобоворимую форму
parsePath :: [Value] -> [Result PNode]
parsePath x = do
    (x, i) <- zip x [0..] 
    return $ if
        | i `mod` 4 == 0 -> PNodeMolecule     <$> parse toMolecule x
        | i `mod` 4 == 1 -> PNodeREAGNT_IN    <$> pure REAGNT_IN
        | i `mod` 4 == 2 -> PNodeReaction     <$> parse toReaction x
        | i `mod` 4 == 3 -> PNodePRODUCT_FROM <$> parse toProduct x

