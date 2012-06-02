

-- UUAGC 0.9.38.6 (./ag/DataSemFuns.ag)


{-# LANGUAGE FlexibleContexts #-}
module DataSemFuns where
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Derived
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances

totalWidth :: Int
totalWidth = 572

toItems :: String -> [Item]
toItems = runParser "Command Parameters" (listParser pItem)
    where pItem :: Parser Item
          pItem = Item <$> pNatural

calculate :: String -> [Field]
calculate input = let items = toItems input
                  in sem_Root (Root items)

instance Show Field where
    show (Field w x) = "width\t: " ++ show w ++ "\n" ++ "xPos\t: " ++ show x ++ "\n"

-- Field -------------------------------------------------------
data Field  = Field (Int) (Int) 
-- cata
sem_Field :: Field  ->
             T_Field 
sem_Field (Field _width _xPos )  =
    (sem_Field_Field _width _xPos )
-- semantic domain
type T_Field  = ( )
sem_Field_Field :: Int ->
                   Int ->
                   T_Field 
sem_Field_Field width_ xPos_  =
    (let 
     in  ( ))
-- Item --------------------------------------------------------
data Item  = Item (Int) 
-- cata
sem_Item :: Item  ->
            T_Item 
sem_Item (Item _value )  =
    (sem_Item_Item _value )
-- semantic domain
type T_Item  = Int ->
               ( Field ,Int)
sem_Item_Item :: Int ->
                 T_Item 
sem_Item_Item value_  =
    (\ _lhsIxPos ->
         (let _lhsOwidth :: Int
              _lhsOitems :: Field 
              _width =
                  (value_ * totalWidth) `div` 100
              _lhsOwidth =
                  _width
              _lhsOitems =
                  Field _width     _lhsIxPos
          in  ( _lhsOitems,_lhsOwidth)))
-- Items -------------------------------------------------------
type Items  = [Item ]
-- cata
sem_Items :: Items  ->
             T_Items 
sem_Items list  =
    (Prelude.foldr sem_Items_Cons sem_Items_Nil (Prelude.map sem_Item list) )
-- semantic domain
type T_Items  = Int ->
                ( ([Field]))
sem_Items_Cons :: T_Item  ->
                  T_Items  ->
                  T_Items 
sem_Items_Cons hd_ tl_  =
    (\ _lhsIxPos ->
         (let _hdOxPos :: Int
              _tlOxPos :: Int
              _lhsOitems :: ([Field])
              _hdIitems :: Field 
              _hdIwidth :: Int
              _tlIitems :: ([Field])
              _hdOxPos =
                  _lhsIxPos
              _tlOxPos =
                  _hdIwidth + _lhsIxPos
              _lhsOitems =
                  _hdIitems : _tlIitems
              ( _hdIitems,_hdIwidth) =
                  hd_ _hdOxPos 
              ( _tlIitems) =
                  tl_ _tlOxPos 
          in  ( _lhsOitems)))
sem_Items_Nil :: T_Items 
sem_Items_Nil  =
    (\ _lhsIxPos ->
         (let _lhsOitems :: ([Field])
              _lhsOitems =
                  []
          in  ( _lhsOitems)))
-- Root --------------------------------------------------------
data Root  = Root (Items ) 
-- cata
sem_Root :: Root  ->
            T_Root 
sem_Root (Root _items )  =
    (sem_Root_Root (sem_Items _items ) )
-- semantic domain
type T_Root  = ( ([Field]))
sem_Root_Root :: T_Items  ->
                 T_Root 
sem_Root_Root items_  =
    (let _itemsOxPos :: Int
         _lhsOitems :: ([Field])
         _itemsIitems :: ([Field])
         _itemsOxPos =
             0
         _lhsOitems =
             _itemsIitems
         ( _itemsIitems) =
             items_ _itemsOxPos 
     in  ( _lhsOitems))