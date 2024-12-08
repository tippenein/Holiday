{-# LANGUAGE OverloadedStrings #-}

module Data.Time.Calendar.BankHoliday.UnitedStatesSpec (spec) where

import Data.List (nub, sort)
import Data.Time
import Test.Hspec
import Test.QuickCheck

import Data.Time.Calendar.BankHoliday (isWeekday, yearFromDay)
import Data.Time.Calendar.BankHoliday.UnitedStates

spec :: Spec
spec = do
  describe "bankHolidays" $ do
    it "are always a weekday" $ property
      $ \yr -> all (\d -> isWeekday d) (bankHolidays yr)

    it "gets all dates in 2017 correct" $ do
      let year = fromGregorian 2017
      let dates = [ year 1 2
                  , year 1 16
                  , year 2 20
                  , year 5 29
                  , year 7 4
                  , year 9 4
                  , year 10 9
                  , year 11 23
                  , year 12 25
                  ]
      (sort (bankHolidays 2017)) `shouldBe` dates

    it "gets all dates in 2021 correct" $ do
      let year = fromGregorian 2021
      let dates = [ year 1 1
                  , year 1 18
                  , year 2 15
                  , year 5 31
                  , year 6 18
                  , year 7 5
                  , year 9 6
                  , year 10 11
                  , year 11 11
                  , year 11 25
                  ]
      (sort (bankHolidays 2021)) `shouldBe` dates

    it "do not include dates before the inception of bank holidays" $ do
      (bankHolidays 1932) `shouldBe` []

    it "falling on a sunday are delegated to following monday" $ do
      (bankHolidays 2017) `shouldContain` [fromGregorian 2017 1 2]

    it "falling on a saturday are open the preceding friday" $ do
      (bankHolidays 2011) `shouldNotContain` [fromGregorian 2011 12 31]

  describe "isBankHoliday" $ do
    it "returns true for the days we expect" $ do
      let christmas = fromGregorian 2015 12 25
      let newYears = fromGregorian 2014 1 1
      let fourth = fromGregorian 2014 7 4
      all isBankHoliday [christmas, newYears, fourth]

  describe "holidaysBetweenYears" $ do
    it "does not include dates outside of range" $ do
      let tooEarly = bankHolidays 1999
      let tooLate = bankHolidays 2017
      let justRight = holidaysBetweenYears 2000 2016
      justRight `shouldNotContain` tooEarly
      justRight `shouldNotContain` tooLate

    it "keeps them in order" $ do
      let oneYearRange = holidaysBetweenYears 2000 2001
      let f = yearFromDay $ head $ oneYearRange
      let l = yearFromDay $ head $ reverse $ oneYearRange
      f `shouldBe` 2000
      l `shouldBe` 2001

    it "does not duplicate if given the same year" $ do
      let sameYearRange = holidaysBetweenYears 2000 2000
      nub sameYearRange `shouldBe` sameYearRange


  describe "holidaysBetween" $ do
    it "does not include dates outside of range" $ do
      let (s, e) = (fromGregorian 2014 1 2, fromGregorian 2014 7 4)
      let theRange = holidaysBetween s e
      theRange `shouldNotContain` [fromGregorian 2014 1 1]
      theRange `shouldNotContain` [fromGregorian 2014 7 5]
      theRange `shouldContain` [e]
      theRange `shouldContain` [fromGregorian 2014 1 20]

  describe "holidays depended on weekdays" $ do
    it "2021" $ do
      bankHolidays 2021 `shouldContain` [fromGregorian 2021 2 15] -- president day
      bankHolidays 2021 `shouldContain` [fromGregorian 2021 11 25] -- thanksgiving day
    it "2022" $ do
      bankHolidays 2022 `shouldContain` [fromGregorian 2022 2 21] -- president day
      bankHolidays 2022 `shouldContain` [fromGregorian 2022 11 24] -- thanksgiving day
    it "2023" $ do
      bankHolidays 2023 `shouldContain` [fromGregorian 2023 2 20] -- president day
      bankHolidays 2023 `shouldContain` [fromGregorian 2023 11 23] -- thanksgiving day
      
