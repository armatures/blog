{-# LANGUAGE NamedFieldPuns #-}
import Control.Applicative
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import GHC.Float
import Data.List.Split (chunksOf)
import Data.List (intersperse)

data TaxSpec = TaxSpec { boundaries :: [Int], rates :: [Float], year :: String}
data GraphSpec = GraphSpec { xMax :: Int, rightMax :: Int, rateMax :: Int }

main :: IO ()
main =
  (renderTaxSpec spec2020 graphSpec2020)
  >> (renderTaxSpec spec1954 graphSpec1954)

renderTaxSpec :: TaxSpec -> GraphSpec -> IO ()
renderTaxSpec spec (GraphSpec {xMax, rightMax, rateMax})=
    let
        incomes = [100,200..xMax]
        graphPoints :: (Int -> Double) -> [(Double, Double)]
        graphPoints f = zip
            (fromIntegral <$> incomes)
            (f <$> incomes)

        scaleXTickLabels :: Int -> String
        scaleXTickLabels = show . (flip div 1000)

        xTickLabels = scaleTicksAndLabel (tail (boundaries spec))
        rightTickLabels = scaleTicksAndLabel [0, 25000..rightMax]

        scaleTicksAndLabel ticks = getZipList $
            (,) <$>
            ZipList (fromIntegral <$> ticks) <*>
            ZipList (fmap scaleXTickLabels ticks)

        yTicks = [0,5..(fromIntegral rateMax)] :: [Double]
        rateTickLabels :: [(Double , String)]
        rateTickLabels = getZipList $
            (,) <$>
            ZipList (flip (/) 100 <$> yTicks) <*>
            ZipList ((flip (++) "%" . show . floor) <$> yTicks)
    in
    toFile def ("static/effectiveRates" ++ (year spec) ++ ".png") $ do
    layoutlr_title .= (year spec ++ " Effective Income Tax Rates")

    layoutlr_left_axis . laxis_override .= (axisGridHide . axisLabelsOverride rateTickLabels)
    layoutlr_left_axis . laxis_title .= "Effective Rate"

    layoutlr_right_axis . laxis_override .= (axisGridHide . axisLabelsOverride rightTickLabels)
    layoutlr_right_axis . laxis_title .= "Total Tax ($000)"

    layoutlr_x_axis . laxis_override .= (axisGridAtLabels . axisLabelsOverride xTickLabels )
    layoutlr_x_axis . laxis_title .= "Income ($000)"

    plotLeft (line "Effective Rate" [graphPoints (float2Double . (effectiveTaxForIncome spec))])
    plotLeft (line "Marginal Rate" [graphPoints (float2Double . (marginalRateForIncome spec))])
    plotRight (line "Total Tax" [graphPoints (float2Double . (taxForIncome spec))])


boundaries1954 = [ 0 , 2000 , 4000 , 6000 , 8000 , 10000 , 12000 , 14000 , 16000 , 18000 , 20000 , 22000 , 26000 , 32000 , 38000 , 44000 , 50000 , 60000 , 70000 , 80000 , 90000 , 100000 , 150000 , 200000 , 600000]
rates1954 = [ 0.20 , 0.22 , 0.26 , 0.30 , 0.34 , 0.38 , 0.43 , 0.47 , 0.50 , 0.53 , 0.56 , 0.59 , 0.62 , 0.65 , 0.69 , 0.72 , 0.75 , 0.78 , 0.81 , 0.84 , 0.87 , 0.89 , 0.90 , 0.91 ]
spec1954 = TaxSpec boundaries1954 rates1954 "1954"
graphSpec1954 = GraphSpec 200000 200000 95

bracketBoundaries :: [Int]
bracketBoundaries = [0, 9875, 40125, 85525, 163300, 207350, 518400, 600000]
bracketRates = [0.10 ,0.12 ,0.22 ,0.24 ,0.32 ,0.35 ,0.37 ]
spec2020 = TaxSpec bracketBoundaries bracketRates "2020"
graphSpec2020 = GraphSpec 600000 400000 40

marginalRateForIncome :: TaxSpec -> Int -> Float
marginalRateForIncome spec i =
  rate . last $ relevantBrackets spec i

effectiveTaxForIncome :: TaxSpec -> Int -> Float
effectiveTaxForIncome spec i =
    (taxForIncome spec i) / (fromIntegral i)

relevantBrackets :: TaxSpec -> Int -> [Bracket]
relevantBrackets (TaxSpec {boundaries, rates} ) i =
  let
    allBrackets = getZipList $ Bracket <$>
            ZipList boundaries <*>
            ZipList (tail boundaries) <*>
            ZipList rates
  in
    takeWhile (\bracket -> (bottom bracket) < i) allBrackets

taxForIncome :: TaxSpec -> Int -> Float
taxForIncome spec i =
    sum $
    (\b->
        let
            upperBound :: Int
            upperBound = min (top b) i
        in
        (fromIntegral $ upperBound - (bottom b))*(rate b)
    )
    <$> (relevantBrackets spec i)


data Bracket = Bracket { bottom :: Int
                        ,top :: Int
                        , rate :: Float
                        } deriving (Show)

