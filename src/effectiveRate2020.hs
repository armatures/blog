import           Control.Applicative
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import GHC.Float
import Data.List.Split (chunksOf)
import Data.List (intersperse)

main :: IO ()
main =
    let
        incomes = [1000,2000..600000]
        graphPoints :: (Int -> Double) -> [(Double, Double)]
        graphPoints f = zip
            (fromIntegral <$> incomes)
            (f <$> incomes)

        scaleXTickLabels :: Int -> String
        scaleXTickLabels = show . (flip div 1000)

        x_ticks = tail bracketBoundaries
        x_labels = fmap scaleXTickLabels x_ticks
        xTickLabels = getZipList $
            (,) <$>
            ZipList (fromIntegral <$> x_ticks) <*>
            ZipList x_labels

        yTicks = [5,10..40]
        rateTickLabels :: [(Double , String)]
        rateTickLabels = getZipList $
            (,) <$>
            ZipList (flip (/) 100 <$> yTicks) <*>
            ZipList ((flip (++) "%" . show . floor) <$> yTicks)
    in
    toFile def "static/effectiveRates2020.png" $ do
    layoutlr_title .= "2020 Effective Income Tax Rates"

    layoutlr_left_axis . laxis_override .= (axisGridHide . axisLabelsOverride rateTickLabels)
    layoutlr_left_axis . laxis_title .= "Effective Rate"

    layoutlr_right_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_title .= "Total Tax"

    layoutlr_x_axis . laxis_override .= (axisGridAtLabels . axisLabelsOverride xTickLabels )
    layoutlr_x_axis . laxis_title .= "Income ($000)"

    plotLeft (line "Effective Rate" [graphPoints (float2Double . effectiveTaxForIncome)])
    plotRight (line "Total Tax" [graphPoints (float2Double . taxForIncome)])


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

