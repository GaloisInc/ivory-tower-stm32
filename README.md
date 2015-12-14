# [ivory-tower-stm32][]

## About

This repository contains both a board support library and a [Tower][]
backend for using the [Ivory][]/[Tower][] languages on the [STM32][] line
of microcontrollers.

[Ivory][] is an embedded domain specific language (EDSL) which aims to provide
a systems-level programming language that removes some common pitfalls of
programming in C, without sacrificing expressivity.

[Tower][] is a concurrency framework for the [Ivory language][ivory]. Tower
composes Ivory programs into monitors which communicate with synchronous
channels.

[![Build Status](https://travis-ci.org/GaloisInc/ivory-tower-stm32.svg?branch=master)](https://travis-ci.org/GaloisInc/ivory-tower-stm32)

## Copyright and license
Copyright 2014 [Galois, Inc.][galois]

Licensed under the BSD 3-Clause License; you may not use this work except in
compliance with the License. A copy of the License is included in the LICENSE
file.

[ivory]: http://github.com/GaloisInc/ivory
[tower]: http://github.com/GaloisInc/tower
[ivory-tower-stm32]: http://github.com/GaloisInc/ivory-tower-stm32
[overview]: http://smaccmpilot.org/software/tower-overview.html

[STM32]: http://www.st.com/stm32
[freertos]: http://freertos.org
[galois]: http://galois.com

## Contributing

This project adheres to the
[Contributor Covenant code of conduct](CODE_OF_CONDUCT.md).
By participating, you are expected to uphold this code. Please report unaccpetable
behavior to [smaccm@galois.com](mailto:smaccm@galois.com).
