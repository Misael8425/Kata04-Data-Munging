-- Importaciones
import Data.Either
import Data.List
import Data.Maybe
import Data.String (split, readMaybe)

-- **Parte Uno: Datos del Clima**

data WeatherData = WeatherData { day :: Int, maxTemp :: Double, minTemp :: Double }

-- Función para analizar los datos del clima
parseWeatherData :: String -> Either String [WeatherData]
parseWeatherData input =
  lines input >>= map (readMaybe WeatherData.parseLine)

-- Función para analizar una línea de datos del clima
parseLine :: String -> Maybe WeatherData
parseLine line =
  let xs = split ',' line
      maybeDay = readMaybe Int (xs !! 0)
      maybeMaxTemp = readMaybe Double (xs !! 1)
      maybeMinTemp = readMaybe Double (xs !! 2)
  in sequence [maybeDay, maybeMaxTemp, maybeMinTemp] >>= WeatherData

-- Función para encontrar el día con la menor diferencia de temperatura
smallestSpreadDay :: [WeatherData] -> Int
smallestSpreadDay data =
  let diffs = map (`abs` . subtract <$> minTemp) data
      minDiff = minimum diffs
  in head (filter ((== minDiff) . diffs) data).day

-- **Parte Dos: Tabla de la Liga de Fútbol**

data TeamData = TeamData { name :: String, goalsFor :: Int, goalsAgainst :: Int }

-- Función para analizar los datos de los equipos de fútbol
parseTeamData :: String -> Maybe TeamData
parseTeamData line =
  let xs = split ',' line
      maybeName = Just (xs !! 0)
      maybeGoalsFor = readMaybe Int (xs !! 1)
      maybeGoalsAgainst = readMaybe Int (xs !! 2)
  in sequence [maybeName, maybeGoalsFor, maybeGoalsAgainst] >>= TeamData

-- Función para encontrar el equipo con la menor diferencia de goles
smallestGoalDifferenceTeam :: [TeamData] -> String
smallestGoalDifferenceTeam data =
  let diffs = map (`abs` . subtract <$> goalsAgainst) data
      minDiff = minimum diffs
  in head (filter ((== minDiff) . diffs) data).name

-- **Parte Tres: Fusión DRY**

data FieldType = Int | Double

data ParsedData = ParsedData { fields :: [FieldType], data :: [String] }

-- Función para analizar datos genéricos
parseData :: String -> Either String (ParsedData, String)
parseData input = do
  let fields = map getFieldByColumnType ["Int", "Double", "Double"]
      lines <- lines input
      parsedLines <- forM lines $ \line -> do
        let xs = split ',' line
        sequence (zipWith getFieldByColumn fields xs)
  (right, _) <- sequence (map snd <$> parsedLines)
  return (ParsedData fields, right)

-- Función para obtener el tipo de campo de una columna
getFieldByColumnType :: [String] -> String -> FieldType
getFieldByColumnType types col = case head (filter ((== col) . snd) (zip [0..] types)) of
  Just (_, t) -> case t of
    "Int" -> Int
    "Double" -> Double
    _ -> error "Invalid field type"
  Nothing -> error "Invalid column name"

-- Función para obtener el valor de un campo
getFieldByColumn :: FieldType -> String -> Either String String
getFieldByColumn t str =
  case readMaybe t str of
    Just x -> Right x
    Nothing -> Left "Failed to parse field"

-- Función para calcular la diferencia mínima
calculateSmallestDifference :: (Double -> Double) -> [ParsedData] -> Maybe String
calculateSmallestDifference diffFunction data = do
  parsedData <- head (filter isCorrectData data)
  let diffs = map (\x -> abs (diffFunction (head x) (last x))) (tail parsedData.data)
      minDiff = minimum diffs
      teamWithMinDiff = head (filter ((== minDiff) . diffs) (tail parsedData.data))
  Just (head parsedData.data !! 0 ++ " with difference " ++ show minDiff)

-- Función para verificar si los datos son correctos
isCorrectData :: ParsedData -> Bool
isCorrectData parsedData =
  length parsedData.fields == length parsedData.data

-- Función principal
main :: IO ()
main = do
  -- **Parte Uno: Datos del Clima**
  contents <- readFile "weather.dat"
  case parseData contents of
    Left err -> putStrLn err
    
