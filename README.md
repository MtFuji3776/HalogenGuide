# HalogenGuide
Examples of PureScript Halogen

https://purescript-halogen.github.io/purescript-halogen/index.html

## 概要

Halogenのコンポーネントは木構造を成しており、隣接するノード同士が通信できる仕組みが備わっている。コンポーネント間のデータ通信は

- ```Input``` : 親コンポーネント→子コンポーネント
- ```Output```: 子→親
- ```Query```: 親→子

の三つから成る。
