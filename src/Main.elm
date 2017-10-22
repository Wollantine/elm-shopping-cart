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

buyThreeGetOneFree: Int -> Float -> Float
buyThreeGetOneFree amount price =
    let
        triplets = toFloat (amount // 3)
        rest = amount % 3
    in
        (triplets * (price * 2)) + (byUnit rest price)

percent: Int -> Int -> Float -> Float
percent discount amount price =
    let
        totalPrice = byUnit amount price
    in
        totalPrice - ((totalPrice * (toFloat discount)) / 100)

byUnit: Int -> Float -> Float
byUnit amount price = (toFloat amount) * price

pack: Int -> Float -> Int -> Float -> Float
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
        rest = case orderedMethods of
            _::r -> r
            [] -> []
    in
        if (isAppliable line method)
            then method
            else appliablePaymentMethod line rest

computePrice: Line -> Float
computePrice line =
    let
        itemPrice = line.item.price
        lineAmount = line.amount
    in
        case (appliablePaymentMethod line line.item.paymentMethodList) of
            BuyThreeGetOneFree -> buyThreeGetOneFree lineAmount itemPrice
            PercentDiscount discount -> percent discount lineAmount itemPrice
            ByUnit -> byUnit lineAmount itemPrice
            Pack size price -> pack size price lineAmount itemPrice
