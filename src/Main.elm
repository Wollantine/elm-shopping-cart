module Main exposing (..)

import List exposing (..)

type alias Item =
    { name: String
    , price: Float
    , paymentMethodList: List PaymentMethod
    }

type alias Line =
    { item: Item
    , amount: Int
    }

type PaymentMethod =
  BuyThreeGetOneFree |
  PercentDiscount Int |
  ByUnit |
  Pack Int Float

methodPriority: PaymentMethod -> Int
methodPriority method =
    case method of
        BuyThreeGetOneFree -> 0
        Pack _ _ -> 1
        PercentDiscount _ -> 2
        ByUnit -> 3

compareMethodPriority: PaymentMethod -> PaymentMethod -> Order
compareMethodPriority a b =
    let
        priorityA = methodPriority a
        priorityB = methodPriority b
    in
        case priorityA - priorityB of
            0 -> EQ
            x -> if x < 0 then LT else GT

type alias PriceComputation = Int -> Float -> Float

buyThreeGetOneFree: PriceComputation
buyThreeGetOneFree amount price =
    pack 3 (price * 2) amount price

percent: Int -> PriceComputation
percent discount amount price =
    let
        totalPrice = byUnit amount price
    in
        totalPrice - ((totalPrice * (toFloat discount)) / 100)

byUnit: PriceComputation
byUnit amount price = (toFloat amount) * price

pack: Int -> Float -> PriceComputation
pack packSize packPrice amount price =
    let
        packs = toFloat (amount // packSize)
        rest = amount % packSize
    in
        (packs * packPrice) + (byUnit rest price)



isAppliable: Line -> PaymentMethod -> Bool
isAppliable line method =
    case method of
        BuyThreeGetOneFree -> line.amount >= 3
        _ -> True

appliablePaymentMethod: Line -> List PaymentMethod -> PaymentMethod
appliablePaymentMethod line methods =
    let
        orderedMethods = sortWith compareMethodPriority methods
        method = case orderedMethods of
            m::_ -> m
            [] -> ByUnit
        rest = Maybe.withDefault [] (tail orderedMethods)
    in
        if (isAppliable line method)
            then method
            else appliablePaymentMethod line rest

computePrice: Line -> Float
computePrice line =
    let
        priceComputation: PriceComputation
        priceComputation = case (appliablePaymentMethod line line.item.paymentMethodList) of
            BuyThreeGetOneFree -> buyThreeGetOneFree
            PercentDiscount discount -> percent discount
            ByUnit -> byUnit
            Pack size price -> pack size price
    in
        priceComputation line.amount line.item.price
