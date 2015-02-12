{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Ivory.BSP.STM32F427.GPIO.Ports
  ( gpioA
  , gpioB
  , gpioC
  , gpioD
  , gpioE
  , gpioF
  , gpioG
  , gpioH
  , gpioI
  , gpioJ
  , gpioK
  ) where

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.Peripheral.GPIOF4.Peripheral

import Ivory.BSP.STM32F427.RCC
import Ivory.BSP.STM32F427.MemoryMap

gpioA :: GPIOPort
gpioA = mkGPIOPort gpioa_periph_base
          (rccEnable rcc_ahb1en_gpioa)
          (rccDisable rcc_ahb1en_gpioa)
          0

gpioB :: GPIOPort
gpioB = mkGPIOPort gpiob_periph_base
          (rccEnable rcc_ahb1en_gpiob)
          (rccDisable rcc_ahb1en_gpiob)
          1

gpioC :: GPIOPort
gpioC = mkGPIOPort gpioc_periph_base
          (rccEnable rcc_ahb1en_gpioc)
          (rccDisable rcc_ahb1en_gpioc)
          2

gpioD :: GPIOPort
gpioD = mkGPIOPort gpiod_periph_base
          (rccEnable rcc_ahb1en_gpiod)
          (rccDisable rcc_ahb1en_gpiod)
          3

gpioE :: GPIOPort
gpioE = mkGPIOPort gpioe_periph_base
          (rccEnable rcc_ahb1en_gpioe)
          (rccDisable rcc_ahb1en_gpioe)
          4

gpioF :: GPIOPort
gpioF = mkGPIOPort gpiof_periph_base
          (rccEnable rcc_ahb1en_gpiof)
          (rccDisable rcc_ahb1en_gpiof)
          5

gpioG :: GPIOPort
gpioG = mkGPIOPort gpiog_periph_base
          (rccEnable rcc_ahb1en_gpiog)
          (rccDisable rcc_ahb1en_gpiog)
          6

gpioH :: GPIOPort
gpioH = mkGPIOPort gpioh_periph_base
          (rccEnable rcc_ahb1en_gpioh)
          (rccDisable rcc_ahb1en_gpioh)
          7

gpioI :: GPIOPort
gpioI = mkGPIOPort gpioi_periph_base
          (rccEnable rcc_ahb1en_gpioi)
          (rccDisable rcc_ahb1en_gpioi)
          8

gpioJ :: GPIOPort
gpioJ = mkGPIOPort gpioj_periph_base
          (rccEnable rcc_ahb1en_gpioj)
          (rccDisable rcc_ahb1en_gpioj)
          9

gpioK :: GPIOPort
gpioK = mkGPIOPort gpiok_periph_base
          (rccEnable rcc_ahb1en_gpiok)
          (rccDisable rcc_ahb1en_gpiok)
          10

rccEnable :: BitDataField RCC_AHB1ENR Bit -> Ivory eff ()
rccEnable f = modifyReg regRCC_AHB1ENR $ setBit f
rccDisable :: BitDataField RCC_AHB1ENR Bit -> Ivory eff ()
rccDisable f = modifyReg regRCC_AHB1ENR $ clearBit f


