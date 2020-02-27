{-# LANGUAGE NamedFieldPuns #-}
import Control.Applicative
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import GHC.Float
import Data.List.Split (chunksOf)
import Data.List (intersperse, nub)

data TaxSpec = TaxSpec { boundaries :: [Int], rates :: [Float], year :: String}
data FileSpec = FileSpec { filename :: String, title :: String }
data GraphSpec = GraphSpec { xMax :: Int, rightMax :: Int, rateMax :: Int }

main :: IO ()
main =
  print (boundaries spec1954adjusted)
  >> renderBothTaxSpec spec2020 spec1954adjusted fileSpec graphSpec

renderBothTaxSpec :: TaxSpec -> TaxSpec -> FileSpec -> GraphSpec -> IO ()
renderBothTaxSpec spec1 spec2 filespec (GraphSpec {xMax, rightMax, rateMax})=
    let
        incomes = [100,200..xMax]
        graphPoints :: (Int -> Double) -> [(Double, Double)]
        graphPoints f = zip
            (fromIntegral <$> incomes)
            (f <$> incomes)

        xTickLabels :: [(Double, String)]
        xTickLabels = scaleTicksAndLabel ([0,100000..2000000])

        scaleTicksAndLabel :: [Int] -> [(Double, String)]
        scaleTicksAndLabel ticks = getZipList $
            (,) <$>
            ZipList (fromIntegral <$> ticks) <*>
            ZipList (fmap (\tick -> show (div tick 1000)) ticks)

        yTicks = [0,5..(fromIntegral rateMax)] :: [Double]
        rateTickLabels :: [(Double , String)]
        rateTickLabels = getZipList $
            (,) <$>
            ZipList (flip (/) 100 <$> yTicks) <*>
            ZipList ((flip (++) "%" . show . floor) <$> yTicks)

        plot_ :: TaxSpec -> EC (Layout Double Double) ()
        plot_ spec = do
          plot (line ("Effective Rate " <> year spec) [graphPoints (float2Double . (effectiveTaxForIncome spec))])
          plot (line ("Marginal Rate " <> year spec) [graphPoints (float2Double . (marginalRateForIncome spec))])
    in

    toFile def ("static/" ++ filename filespec ++ ".png") $ do
    layout_title .= title filespec

    layout_y_axis . laxis_override .= (axisGridHide . axisLabelsOverride rateTickLabels)
    layout_y_axis . laxis_title .= "Effective Rate"

    plot_ spec2
    -- plot_ spec1

    layout_x_axis . laxis_override .= (axisLabelsOverride xTickLabels)
    layout_x_axis . laxis_title .= "Income ($000)"


cpiAdjuster1954 = 369.8/45.2 :: Double -- 2018 dollars, using the Bureau of Labor Statistics' (BLS) Consumer Price Index Research Series (CPI-U-RS) from https://www.census.gov/topics/income-poverty/income/guidance/current-vs-constant-dollars.html
boundaries1954 = [ 0 , 2000 , 4000 , 6000 , 8000 , 10000 , 12000 , 14000 , 16000 , 18000 , 20000 , 22000 , 26000 , 32000 , 38000 , 44000 , 50000 , 60000 , 70000 , 80000 , 90000 , 100000 , 150000 , 200000 , 205000 , 205000] :: [Int]
rates1954 = [ 0.20 , 0.22 , 0.26 , 0.30 , 0.34 , 0.38 , 0.43 , 0.47 , 0.50 , 0.53 , 0.56 , 0.59 , 0.62 , 0.65 , 0.69 , 0.72 , 0.75 , 0.78 , 0.81 , 0.84 , 0.87 , 0.89 , 0.90 , 0.91 , 0.91 ]

boundaries1954adjusted = fmap (floor . (* cpiAdjuster1954) . fromIntegral) boundaries1954
spec1954adjusted = TaxSpec boundaries1954adjusted rates1954 "1954"

bracketBoundaries :: [Int]
bracketBoundaries = [0,    9875, 40125, 85525, 163300, 207350, 518400, 2000000]
bracketRates =      [0.10, 0.12, 0.22,  0.24,  0.32,   0.35,   0.37,   0.37]
spec2020 = TaxSpec bracketBoundaries bracketRates "2020"
graphSpec = GraphSpec 600000 400000 100
fileSpec = FileSpec "ratesComparison1954vs2020v3" "1954 and 2020 Income Tax Rates"

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

