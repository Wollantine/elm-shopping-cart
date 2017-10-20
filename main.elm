module Main exposing (..)

import List exposing (..)

type alias Item =
    { name: String
    , price: Int
    , paymentMethodList: List PaymentMethod
    }

type alias Line =
    { item: Item
    , amount: Int
    }

type PaymentMethod = BuyThreeGetOneFree | PercentDiscount Int | ByUnit

methodPriority: PaymentMethod -> Int
methodPriority method =
    case method of
        BuyThreeGetOneFree -> 0
        PercentDiscount _ -> 1
        ByUnit -> 2

compareMethodPriority: PaymentMethod -> PaymentMethod -> Order
compareMethodPriority a b =
    let
        priorityA = methodPriority a
        priorityB = methodPriority b
    in
        case priorityA - priorityB of
            0 -> EQ
            x -> if x < 0 then LT else GT

buyThreeGetOneFree: Int -> Int -> Int
buyThreeGetOneFree amount price =
    let
        triplets = amount // 3
        rest = amount % 3
    in
        (triplets * (price * 2)) + (byUnit price rest)

percent: Int -> Int -> Int -> Int
percent discount amount price =
    let
        totalPrice = byUnit amount price
    in
        totalPrice - ((totalPrice * discount) // 100)

byUnit: Int -> Int -> Int
byUnit amount price = amount * price

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

computePrice: Line -> Int
computePrice line =
    case (appliablePaymentMethod line line.item.paymentMethodList) of
        BuyThreeGetOneFree -> buyThreeGetOneFree line.amount line.item.price
        PercentDiscount discount -> percent discount line.amount line.item.price
        ByUnit -> byUnit line.amount line.item.price

