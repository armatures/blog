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

        x_ticks = tail bracketBoundaries
        x_labels' = fmap (show . (flip div 1000)) x_ticks
        x_labels = getZipList $
            (,) <$>
            ZipList (fromIntegral <$> x_ticks) <*>
            ZipList x_labels'
    in
    toFile def "static/effectiveRates2020.png" $ do
    layout_title .= "2020 Effective Income Tax Rates"

    layout_y_axis . laxis_override .= axisGridHide
    layout_y_axis . laxis_title .= "Effective Rate"

    layout_x_axis . laxis_override .= (axisGridAtLabels . axisLabelsOverride x_labels )
    layout_x_axis . laxis_title .= "Income ($000)"

    plot (line "single" [graphPoints])


bracketBoundaries :: [Int]
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

