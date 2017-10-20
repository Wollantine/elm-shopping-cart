module MainSpec exposing (..)

import Test exposing (..)
import Expect exposing (..)
import ElmTestBDDStyle exposing (..)

import Main exposing (..)

suite: Test
suite =
    describe "Main"
        [ describe "computePrice"
            [ it "returns 3x2 price if method is 3x2" <|
                let
                    item = Item "test" 10 [BuyThreeGetOneFree]
                    line = Line item 3
                in
                    expect (computePrice line) to equal 20
            , it "returns % price if method is %" <|
                let
                    item = Item "test" 5 [PercentDiscount 50]
                    line = Line item 3
                in
                    expect (computePrice line) to equal 8
            , it "returns price by unit if method is by unit" <|
                let
                    item = Item "test" 3 [ByUnit]
                    line = Line item 3
                in
                    expect (computePrice line) to equal 9
            , it "returns 3x2 price if there are more than 2 items and has a 3x2 discount" <|
                let
                    item = Item "test" 10 [PercentDiscount 5, BuyThreeGetOneFree]
                    line = Line item 3
                in
                    expect (computePrice line) to equal 20
            , it "returns the most prioritary discount no matter the order" <|
                let
                    item = Item "test" 10 [BuyThreeGetOneFree, PercentDiscount 5]
                    line = Line item 3
                in
                    expect (computePrice line) to equal 20
            , it "returns the most prioritary appliable discount" <|
                let
                    item = Item "test" 10 [BuyThreeGetOneFree, PercentDiscount 10, ByUnit]
                    line = Line item 2
                in
                    expect (computePrice line) to equal 18
            ]

        , describe "buyThreeGetOneFree"
            [ it "returns price by unit if units < 2" <|
                expect (buyThreeGetOneFree 2 50) to equal 100
            , it "returns zero if amount is zero" <|
                expect (buyThreeGetOneFree 0 100) to equal 0
            , it "returns the price for 2 if amount is 3" <|
                expect (buyThreeGetOneFree 3 50) to equal 100
            , it "returns the price of 5 if amount is 7" <|
                expect (buyThreeGetOneFree 7 1) to equal 5
            ]

        , describe "percent"
            [ it "returns half the price if discount is 50%" <|
                expect (percent 50 4 10) to equal 20
            , it "returns 75% of the price if discount is 25%" <|
                expect (percent 25 10 10) to equal 75
            , it "returns 20% of the price if discount is 80%" <|
                expect (percent 80 10 10) to equal 20
            , it "rounds up" <|
                expect (percent 50 5 1) to equal 3
            ]

        , describe "byUnit"
            [ it "returns the amount times the price" <|
                expect (byUnit 4 5) to equal 20
            , it "returns zero if the amount is zero" <|
                expect (byUnit 0 10) to equal 0
            ]

        , describe "compareMethodPriority"
            [ it "returns LT if a is 3x2" <|
                expect (compareMethodPriority BuyThreeGetOneFree (PercentDiscount 10)) to equal LT
            , it "returns EQ if both are %" <|
                expect (compareMethodPriority (PercentDiscount 10) (PercentDiscount 20)) to equal EQ
            , it "returns GT if a is by unit" <|
                expect (compareMethodPriority ByUnit BuyThreeGetOneFree) to equal GT
            , it "returns EQ if both are by unit" <|
                expect (compareMethodPriority ByUnit ByUnit) to equal EQ
            ]

        , describe "appliablePaymentMethod"
            [ it "returns the first method in the list" <|
                let
                    methods = [PercentDiscount 20, ByUnit]
                    item = Item "a" 1 methods
                    line = Line item 1
                in
                    expect (appliablePaymentMethod line methods) to equal (PercentDiscount 20)
            , it "returns ByUnit if list is empty" <|
                let
                    methods = []
                    item = Item "a" 1 methods
                    line = Line item 1
                in
                    expect (appliablePaymentMethod line methods) to equal ByUnit
            , it "returns the second method if the first is not appliable" <|
                let
                    methods = [BuyThreeGetOneFree, PercentDiscount 10]
                    item = Item "a" 1 methods
                    line = Line item 1
                in
                    expect (appliablePaymentMethod line methods) to equal (PercentDiscount 10)
            , it "orders the method list by priority (3x2, %, and lastly by unit)" <|
                let
                    methods = [PercentDiscount 10, BuyThreeGetOneFree]
                    item = Item "a" 1 methods
                    line = Line item 3
                in
                    expect (appliablePaymentMethod line methods) to equal BuyThreeGetOneFree
            ]

        , describe "isAppliable"
            [ it "returns true when method is 3x2 and there are three items" <|
                let
                    item = Item "test" 5 [BuyThreeGetOneFree]
                    line = Line item 3
                in
                    expect (isAppliable line BuyThreeGetOneFree) to equal True
            , it "returns false when method is 3x2 and there are two items" <|
                let
                    item = Item "test" 5 [BuyThreeGetOneFree]
                    line = Line item 2
                in
                    expect (isAppliable line BuyThreeGetOneFree) to equal False
            , it "returns true when method is %" <|
                let
                    item = Item "a" 1 [PercentDiscount 20]
                    line = Line item 1
                in
                    expect (isAppliable line (PercentDiscount 20)) to equal True
            , it "returns true when method is by unit" <|
                let
                    item = Item "a" 1 [ByUnit]
                    line = Line item 1
                in
                    expect (isAppliable line ByUnit) to equal True
            ]
        ]
