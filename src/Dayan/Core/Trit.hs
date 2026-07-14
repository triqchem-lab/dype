-- | Dayan.Core.Trit — GF(3) 三进制基本单元
--
-- 编码与 Agda GF3.Trit 一致:
--   N (T0) = -1  →  吸收态
--   Z (T1) =  0  →  平衡态
--   P (T2) = +1  →  表达态
--
-- 哲学约束 (宪法层级):
--   GF(3) 的合法身份 = 模 3 整数算术 + 驻波叠加表
--   禁止用于"有限域"语义或"三进制计算机"实现

module Dayan.Core.Trit where
