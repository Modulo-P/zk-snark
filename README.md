# Bringing *ZK-SNARKs* to Cardano


## About

We intend to bring the power of *zk-snarks* to Cardano, concentrating in the Groth16 protocol.  This is a type of zero-knowledge proof that has become popular in the Ethereum blockchain.

Broadly speaking, zero-knowledge-proofs have the (perhaps surprising) property of allowing to prove the possesion of some information without revealing the information itself.  This promises to have applications in many important areas of cryptography, finance, privacy-rights, etc.

Our project attempts to:

- Implement Miller's algorithm in Plutus
- Define a Weil's Pairing for the BN128 Elliptic Curve
- Implement the Groth16 protocol
- Run the verification part of the Groth16 protocol in a Hydra head
- Give proof-of-concept examples illustrating some practical applications

It should be noticed that the development shown in this repository will be initially baseed on progress achieved in our participation in the Emurgo Build 2013 Hackathon.  (Most of the code of that project is currrently mantained private and will be released upon completion of the arbiter's evaluation.)

## Miller's algorithm

Miller's algorithm is the basis of Weil's pairing, a fundamental building block of the Groth16 protocol.  Directory [miller](./miller) contains a basic implementation in Haskell.

Haskell is particularly well suited since operations on various algebraic structures can be implemented as instances of classes defined in PlutusTx and other classes representing abstract algebra structures like Euclidean Rings, Fields, Elliptic Curves, etc. that we define.