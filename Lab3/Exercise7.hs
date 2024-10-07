

{-

Time spent: 70 minutes

For this example, I will be using R = [(1,2), (2,3)]

Symmetric closure of the transitive closure of a relation R:
    transitive closure: [(1,2), (1,3), (2,3)]
    symmetric closure: [(1,2), (2,1), (1,3), (3,1), (2,3), (3,2)]

Transitive closure of the symmetric closure of R
    symmetric closure: [(1,2), (2,1), (2,3), (3,2)]
    transitive closure: [(1,2), (1,1), (1,3), (2,1), (2,2), (2,3), (3,2), (3,1), (3,3)]
    removing the reflexive pairs: [(1,2), (1,3), (2,1), (2,3), (3,2), (3,1)]

The two transformations result in an identical relation. 

The question we are answering is "Is there a difference between the two?" not the question of "Are the results
different?". The answer to the first quesiton is yes, there are differences between the symmetric closure 
of the transitive closure of relation R and the transitive closure of the symmetric closure of R even though they 
result in the same answer. 

The differences lie in the appearence of reflexive pairs in the transitive closure of the symmetric closure. The 
definition of the symmetric closure states: ∀x, y ∈ A, xRy ⇔ yRx, meaning that it does not add reflexive pairs (xRx) 
unless they are part of the original relation. The definition of the transitive closure states: 
∀x, y, z ∈ A xRy ∧ yRz ⇒ xRz, again not supporting the reflexive.

To conclude, there is a difference between the two despite their result being the same.

-}