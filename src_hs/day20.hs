import Debug.Trace
import qualified Data.Array as A
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Bifunctor as Bi

solveP1 :: String -> Int
solveP1 = product . corners . initEdges . parseTiles . T.pack

type TileId = Int
type Tile = A.Array (Int, Int) Char

parseTiles :: T.Text -> [(TileId, Tile)]
parseTiles = map parseTile . separateTiles

  where separateTiles :: T.Text -> [T.Text]
        separateTiles = T.splitOn (T.pack "\n\n")

        parseTile :: T.Text -> (TileId, Tile)
        parseTile txt = (id, tile)

          where idStr : rows = T.lines txt
                id = read . T.unpack . T.init . (!! 1) . T.words $ idStr
                tile = A.listArray ((0, 0), (m-1, n-1)) (T.unpack $ T.concat rows)

                  where m = length rows
                        n = T.length (head rows)

type Border = String
type Edges = M.Map Border [TileId]

initEdges :: [(TileId, Tile)] -> Edges
initEdges = foldIntoMap . addReversedEdges . concatMap toBorders

  where toBorders :: (TileId, Tile) -> [(Border, TileId)]
        toBorders (id, tile) = zip borders (repeat id)

          where borders = map getBorder [ ((0, 0), (lastX,0))
                                        , ((lastX, 0), (lastX, lastY))
                                        , ((lastX, lastY), (0, lastY))
                                        , ((0, lastY), (0, 0))
                                        ]
                  where getBorder ((startX, startY), (endX, endY))
                          = [tile A.! ind | ind <- zip (range startX endX) (range startY endY)]
                        range a b | a == b = repeat a
                                  | a < b = [a..b]
                                  | otherwise = [a,(a-1)..b]
                        (_, (lastX, lastY)) = A.bounds tile

        addReversedEdges :: [(Border, TileId)] -> [(Border, TileId)]
        addReversedEdges xs = map (Bi.first reverse) xs ++ xs

        foldIntoMap :: [(Border, TileId)] -> Edges
        foldIntoMap = foldr addBorder M.empty

          where addBorder :: (Border, TileId) -> Edges -> Edges
                addBorder (border, id) edges = M.insert border ids' edges
                  where ids' = case M.lookup border edges of Just ids -> id : ids
                                                             Nothing  -> [id]

corners :: Edges -> [TileId]
corners = traceShowId . extractCorners . traceShowId . countTileIds . filter borderIsShared . M.assocs
  where borderIsShared :: (Border, [TileId]) -> Bool
        borderIsShared (_, ids) = length ids > 1

        countTileIds :: [(Border, [TileId])] -> M.Map TileId Int
        countTileIds = foldr addId M.empty . concatMap snd

          where addId :: TileId -> M.Map TileId Int -> M.Map TileId Int
                addId id counts = case M.lookup id counts of
                  Just prevCount -> M.insert id (prevCount + 1) counts
                  Nothing        -> M.insert id 1 counts

        extractCorners :: M.Map TileId Int -> [TileId]
        extractCorners = map fst . filter has2Neighbors . M.assocs
          where has2Neighbors :: (TileId, Int) -> Bool
                has2Neighbors (_, count) = count == 4

expectEq :: (Eq a, Show a) => a -> a -> String -> IO ()
expectEq a b test = do
  if a /= b then putStrLn $ "FAILED " ++ test++ "\nresult: " ++ show a ++ "\nexpected: " ++ show b
            else putStrLn $ "PASS " ++ test
  putStrLn ""

main :: IO()
main = do
  putStrLn "hi"
  example <- readFile "../example.input"
  expectEq (solveP1 example) 20899048083289 "p1 example"
  batch <- readFile "../batch.input"
  print $ solveP1 batch
  putStrLn "bye"
