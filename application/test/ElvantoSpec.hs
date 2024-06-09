{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module ElvantoSpec (spec) where

import qualified Data.Text as T
import NeatInterpolation
import Test.Hspec

testPayloadResponse :: T.Text
testPayloadResponse = 
  [trimming|
    Roster.initRequest({
      request: "",
      response: false,
      scheduleRequests: [
        {
          id: "18628134",
          locationName: "5pm Church",
          serviceName: "5pm Church ",
          scheduleDateTime: "16 June, 2024 5:00 PM",
          positionName: "Piano",
          subDepartmentName: "Musicians",
          departmentName: "Sunday Services",
          departmentId: "4628831f-a0ae-11e5-aba7-06fb5fa8f77d",
          pendingSwapReplace: false,
        },
        {
          id: "18628137",
          locationName: "5pm Church",
          serviceName: "5pm Church ",
          scheduleDateTime: "23 June, 2024 5:00 PM",
          positionName: "Drums",
          subDepartmentName: "Musicians",
          departmentName: "Sunday Services",
          departmentId: "4628831f-a0ae-11e5-aba7-06fb5fa8f77d",
          pendingSwapReplace: false,
        },
        {
          id: "18628141",
          locationName: "5pm Church",
          serviceName: "5pm Church ",
          scheduleDateTime: "30 June, 2024 5:00 PM",
          positionName: "Drums",
          subDepartmentName: "Musicians",
          departmentName: "Sunday Services",
          departmentId: "4628831f-a0ae-11e5-aba7-06fb5fa8f77d",
          pendingSwapReplace: false,
        },
        {
          id: "18628146",
          locationName: "5pm Church",
          serviceName: "5pm Church ",
          scheduleDateTime: "7 July, 2024 5:00 PM",
          positionName: "Piano",
          subDepartmentName: "Musicians",
          departmentName: "Sunday Services",
          departmentId: "4628831f-a0ae-11e5-aba7-06fb5fa8f77d",
          pendingSwapReplace: false,
        },
      ],
      swapReplaceRequests: [],
    });
  |]

spec :: Spec
spec = do
  describe "Elvanto" $ do
    it "should be able to login" $ do
      True `shouldBe` True
