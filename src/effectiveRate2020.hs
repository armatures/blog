import           Control.Applicative
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import GHC.Float

main :: IO ()
main =
    let
        incomes = [100,200..600000]
        graphPoints :: [(Double, Double)]
        graphPoints = zip
            (fromIntegral <$> incomes)
            (float2Double . effectiveTaxForIncome <$> incomes)
    in
    toFile def "static/effectiveRates2020.png" $ do
    layout_title .= "2020 Effective Income Tax Rates"
    setColors [opaque blue, opaque red]
    plot (line "single" [graphPoints])

bracketBoundaries = [0, 9875, 40125, 85525, 163300, 207350, 518400, 600000]
bracketRates = [0.10 ,0.12 ,0.22 ,0.24 ,0.32 ,0.35 ,0.37 ]
allBrackets = getZipList $ Bracket <$>
            ZipList bracketBoundaries <*>
            ZipList (tail bracketBoundaries) <*>
            ZipList bracketRates

relevantBrackets :: Int -> [Bracket]
relevantBrackets i =
        takeWhile (\x -> (bottom x) < i) allBrackets

effectiveTaxForIncome :: Int -> Float
effectiveTaxForIncome i =
    (taxForIncome i) / (fromIntegral i)

taxForIncome :: Int -> Float
taxForIncome i =
    sum $
    (\b->
        let
            upperBound :: Int
            upperBound = min (top b) i
        in
        (fromIntegral $ upperBound - (bottom b))*(rate b)
    )
    <$> (relevantBrackets i)


data Bracket = Bracket { bottom :: Int
                        ,top :: Int
                        , rate :: Float
                        } deriving (Show)

