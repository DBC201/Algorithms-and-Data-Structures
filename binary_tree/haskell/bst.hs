-- Deniz Cakiroglu dbc42
-- Binary tree implementation in haskell
data Tree a = EmptyLeaf | Node a (Tree a) (Tree a)

member :: Ord a => Eq a => a -> Tree a -> Bool
member m EmptyLeaf = False
member m (Node val left right) = 
 if val == m then
  True
 else if m < val then
  member m left
 else
  member m right

insert :: Ord a => a -> Tree a -> Tree a
insert m EmptyLeaf = Node m EmptyLeaf EmptyLeaf
insert m (Node val left right) =
 if m == val then
  Node val left right
 else if m < val then
  Node val (insert m left) right
 else
  Node val left (insert m right)

levels :: Tree a -> Int
levels EmptyLeaf = 0
levels (Node val left right) = 
 if (levels left) > (levels right) then
  (levels left) + 1
 else
  (levels right) + 1

root = insert 4 EmptyLeaf
bst2 = insert 2 root
bst6 = insert 6 bst2
bst1 = insert 1 bst6
bst3 = insert 3 bst1
bst5 = insert 5 bst3
bst7 = insert 7 bst5

