--
-- UART.hs --- UART Driver
--
-- Copyright (C) 2013, Galois, Inc.
-- All Rights Reserved.
--


module Ivory.BSP.STM32F427.UART
  ( uart1, uart2, uart3, uart4, uart5, uart6, uart7, uart8
  ) where

import Ivory.BSP.STM32.Peripheral.UART

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32.ClockConfig

import Ivory.BSP.STM32F427.RCC
import Ivory.BSP.STM32F427.MemoryMap
import qualified Ivory.BSP.STM32F427.Interrupt as F427

uart1, uart2, uart3, uart4, uart5, uart6, uart7, uart8 :: UART
uart1 = mkUART uart1_periph_base
                rccenable rccdisable
                F427.USART1 PClk2 "uart1"
  where
  rccenable  = modifyReg regRCC_APB2ENR $ setBit   rcc_apb2en_uart1
  rccdisable = modifyReg regRCC_APB2ENR $ clearBit rcc_apb2en_uart1

uart2 = mkUART uart2_periph_base
                rccenable rccdisable
                F427.USART2 PClk1 "uart2"
  where
  rccenable  = modifyReg regRCC_APB1ENR $ setBit   rcc_apb1en_uart2
  rccdisable = modifyReg regRCC_APB1ENR $ clearBit rcc_apb1en_uart2

uart3 = mkUART uart3_periph_base
                rccenable rccdisable
                F427.USART3 PClk1 "uart3"
  where
  rccenable  = modifyReg regRCC_APB1ENR $ setBit   rcc_apb1en_uart3
  rccdisable = modifyReg regRCC_APB1ENR $ clearBit rcc_apb1en_uart3

uart4 = mkUART uart4_periph_base
                rccenable rccdisable
                F427.UART4 PClk1 "uart4"
  where
  rccenable  = modifyReg regRCC_APB1ENR $ setBit   rcc_apb1en_uart4
  rccdisable = modifyReg regRCC_APB1ENR $ clearBit rcc_apb1en_uart4

uart5 = mkUART uart5_periph_base
                rccenable rccdisable
                F427.UART5 PClk1 "uart5"
  where
  rccenable  = modifyReg regRCC_APB1ENR $ setBit   rcc_apb1en_uart5
  rccdisable = modifyReg regRCC_APB1ENR $ clearBit rcc_apb1en_uart5

uart6 = mkUART uart6_periph_base
                rccenable rccdisable
                F427.USART6 PClk2 "uart6"
  where
  rccenable  = modifyReg regRCC_APB2ENR $ setBit   rcc_apb2en_uart6
  rccdisable = modifyReg regRCC_APB2ENR $ clearBit rcc_apb2en_uart6

uart7 = mkUART uart7_periph_base
                rccenable rccdisable
                F427.UART7 PClk1 "uart7"
  where
  rccenable  = modifyReg regRCC_APB1ENR $ setBit   rcc_apb1en_uart7
  rccdisable = modifyReg regRCC_APB1ENR $ clearBit rcc_apb1en_uart7

uart8 = mkUART uart8_periph_base
                rccenable rccdisable
                F427.UART8 PClk1 "uart8"
  where
  rccenable  = modifyReg regRCC_APB1ENR $ setBit   rcc_apb1en_uart8
  rccdisable = modifyReg regRCC_APB1ENR $ clearBit rcc_apb1en_uart8

