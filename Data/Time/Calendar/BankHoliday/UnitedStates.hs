------------------------------------------------------------------------------
-- Module      : Data.Time.Calendar.BankHoliday.UnitedStates
-- Maintainer  : brady.ouren@gmail.com
------------------------------------------------------------------------------

module Data.Time.Calendar.BankHoliday.UnitedStates
  (
    isBankHoliday
  , bankHolidays
  , holidaysBetween
  , holidaysBetweenYears
  ) where

import Data.Maybe
import Data.Time (Day, fromGregorian)
import Data.Time.Calendar (DayOfWeek (..), addDays, toModifiedJulianDay, dayOfWeek)
import Data.Time.Calendar.BankHoliday (yearFromDay)

{- | bank holidays for a given year -}
bankHolidays :: Integer -> [Day]
bankHolidays year = filterHistoric standardHolidays
  where
    [jan, feb, jun, jul, sep, oct, nov, dec] = monthsMap
    monthsMap = map (fromGregorian year) [1,2,6,7,9,10,11,12]
    standardHolidays = [
        2 `weeksAfter` firstMondayIn jan -- mlk Day
      , 2 `weeksAfter` firstMondayIn feb   -- presidents day
      , weekBefore (firstMondayIn jun)     -- memorial day
      , firstMondayIn sep                  -- labor day
      , weekAfter (firstMondayIn oct)      -- columbusDay
      , 3 `weeksAfter` firstThursdayIn nov -- thanksgiving
      ] ++ catMaybes [
        weekendHolidayFrom (jan 1)  -- newYearsDay
      , filterHistoricAndGetClosestWeekday (jun 19) -- juneteenth
      , weekendHolidayFrom (jul 4)  -- independenceDay
      , weekendHolidayFrom (nov 11) -- veteransDay
      , weekendHolidayFrom (dec 25) -- christmas
      ]

{- | whether the given day is a bank holiday -}
isBankHoliday :: Day -> Bool
isBankHoliday d = d `elem` bankHolidays (yearFromDay d)

-- | day federal bank holidays were announced in the United States
-- | March 9th 1933
filterHistoric :: [Day] -> [Day]
filterHistoric = filter (> marchNinth1933)
  where marchNinth1933 = fromGregorian 1933 3 9

-- | find holidays falling between 2 years of time
holidaysBetweenYears :: Integer -> Integer -> [Day]
holidaysBetweenYears startYear endYear =
  concatMap bankHolidays [startYear..endYear]

-- | find holidays falling between 2 specific days
holidaysBetween :: Day -> Day -> [Day]
holidaysBetween start end =
  filter (\a -> a >= start && a <= end) fullRange
  where
    fullRange = holidaysBetweenYears (yearFromDay start) (yearFromDay end)

weekendHolidayFrom :: Day -> Maybe Day
weekendHolidayFrom d = case weekIndex d of
  3 -> Nothing            -- saturday
  4 -> Just (addDays 1 d) -- sunday
  _ -> Just d

-- | relative day helper functions
weekIndex day = toModifiedJulianDay day `mod` 7
firstMondayIn month = firstDayOfWeekOnAfter Monday (month 01)
firstThursdayIn month = firstDayOfWeekOnAfter Thursday (month 01)
weeksBefore n = addDays (n * (-7))
weekBefore = weeksBefore 1
weeksAfter n = addDays (n * 7)
weekAfter = weeksAfter 1

firstDayOfWeekOnAfter :: DayOfWeek -> Day -> Day
firstDayOfWeekOnAfter dw d = if dayOfWeek d == dw 
    then d 
    else firstDayOfWeekOnAfter dw  (addDays 1 d)

-- Juneteenth came into effect in 2021
filterHistoricAndGetClosestWeekday :: Day -> Maybe Day
filterHistoricAndGetClosestWeekday d
  | d < fromGregorian 2021 1 1 = Nothing
  | otherwise = case weekIndex d of
    3 -> Just (addDays (-1) d) -- if saturday, use friday
    4 -> Just (addDays 1 d) -- if sunday, use monday
    _ -> Just d -- this is a weekday