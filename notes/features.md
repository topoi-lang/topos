

## Key Features

* purely functional
* dependent type
    - via Normalization by Evaluation

## Feature that I am interested

* non-strict evaluation on special constructor
    - allow user to know when is the allocation, and allocation is transparent to user.

* linear type / monadic region / dynamic linear type
    - to eliminate garbage collections
    - how to make it faster, as it is bad for memory sharing, maybe hash-consing? (edwardkmett/intern)

* dependent record
    - I think this can be integrated to the module system, it sounds cool when the module system is actually a record.

* row polymorphism
    - There is no research on integrating this to the dependent record / inductive data types.

## Implementation feature

* Normalization by Evaluation

* non-strict evaluation on special constructor
    - allow user to know when is the allocation, and allocation is transparent to user.

* unboxed value - fast