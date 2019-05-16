{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Quartz.Syntax.ParQuartz where
import Quartz.Syntax.AbsQuartz
import Quartz.Syntax.LexQuartz
import Quartz.Syntax.ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn32 (String)
	| HappyAbsSyn33 (Integer)
	| HappyAbsSyn34 (Double)
	| HappyAbsSyn35 (QIdent)
	| HappyAbsSyn36 (CustomOperator)
	| HappyAbsSyn37 (QualifiedIdentifier)
	| HappyAbsSyn38 (Program)
	| HappyAbsSyn39 ([Import])
	| HappyAbsSyn40 (Import)
	| HappyAbsSyn41 (Declaration)
	| HappyAbsSyn42 (DataCase)
	| HappyAbsSyn43 ([Type])
	| HappyAbsSyn44 ([DataCase])
	| HappyAbsSyn45 (Arg)
	| HappyAbsSyn46 (TypeQualifier)
	| HappyAbsSyn47 ([TypeQualifier])
	| HappyAbsSyn48 ([Declaration])
	| HappyAbsSyn49 ([Arg])
	| HappyAbsSyn50 (Type)
	| HappyAbsSyn54 (Exp)
	| HappyAbsSyn59 ([Exp])
	| HappyAbsSyn61 (Case)
	| HappyAbsSyn62 ([Case])
	| HappyAbsSyn63 ([QIdent])
	| HappyAbsSyn64 (DoClause)
	| HappyAbsSyn65 ([DoClause])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,785) ([0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,519,0,0,0,0,0,0,128,0,0,0,6144,0,16384,0,0,0,0,0,0,32,0,0,0,0,0,4096,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,12288,0,32768,0,0,0,0,24,0,64,0,0,0,3072,0,8192,0,0,0,0,6,0,16,0,0,0,17152,17856,3873,0,0,0,32768,24609,37026,7,0,0,0,192,20784,968,0,0,0,24576,38912,58408,1,0,0,0,48,5196,242,0,0,0,6144,11778,30986,0,0,0,0,268,34071,60,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,49152,28688,51281,3,0,0,0,2144,10424,484,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2176,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,48,5196,498,0,0,0,0,0,0,0,0,0,0,320,0,0,0,0,0,4096,4,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,2144,10424,484,0,0,0,0,0,0,0,0,0,0,0,0,48,0,0,0,0,0,0,0,0,0,0,32902,17035,30,0,0,0,0,0,2048,0,0,0,32768,57377,37026,7,0,0,0,0,0,8,0,0,0,24576,38912,58408,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,33280,0,0,0,0,0,0,10,0,0,0,0,0,0,0,0,0,0,0,6144,9728,30986,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,0,0,2,0,0,0,0,0,0,0,0,0,12288,0,32768,0,0,0,0,0,0,0,0,0,0,3072,0,8192,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,16608,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,96,0,256,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,1,0,0,0,0,0,256,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,33216,0,0,0,0,0,0,0,0,0,0,0,18432,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,48,0,128,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,2,0,0,0,96,0,256,0,0,0,12288,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,20480,0,0,0,0,0,32768,24577,37026,7,0,0,0,192,20784,968,0,0,0,24576,38912,58408,1,0,0,0,48,5196,242,0,0,0,6144,11778,30986,0,0,0,0,0,0,32,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,28688,52319,3,0,0,0,16640,0,1,0,0,0,12288,23556,61972,0,0,0,0,0,8192,0,0,0,0,0,2,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2304,0,0,0,0,0,24576,47112,58408,1,0,0,0,1072,5212,242,0,0,0,6144,11778,30986,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,28688,51281,3,0,0,0,0,0,0,0,0,0,12288,23556,61972,0,0,0,0,0,0,1,0,0,0,0,0,512,0,0,0,0,32774,17033,30,0,0,0,0,0,128,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2080,0,0,0,0,0,4096,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,32768,16385,0,0,0,0,0,64,32,0,0,0,0,24576,0,0,1,0,0,0,1072,5212,242,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,8,0,0,0,0,0,1024,0,0,0,0,256,0,0,0,0,0,0,0,256,0,0,0,12288,23556,61972,0,0,0,0,536,2606,121,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1536,0,0,0,0,12,34067,60,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,0,0,2,0,0,0,128,0,0,0,0,0,0,2,0,0,0,0,0,536,2606,121,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,4,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1072,5212,242,0,0,0,6144,0,16384,0,0,0,0,16,0,0,0,0,0,0,8,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,49152,28688,51281,3,0,0,0,96,0,256,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,3072,5889,15493,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pQualifiedIdentifier","%start_pProgram","%start_pListImport","%start_pImport","%start_pDeclaration","%start_pDataCase","%start_pListType","%start_pListDataCase","%start_pArg","%start_pTypeQualifier","%start_pListTypeQualifier","%start_pListDeclaration","%start_pListArg","%start_pType2","%start_pType","%start_pListType2","%start_pType1","%start_pExp1","%start_pExp2","%start_pExp5","%start_pExp3","%start_pExp4","%start_pListExp","%start_pExp","%start_pCase","%start_pListCase","%start_pListQIdent","%start_pDoClause","%start_pListDoClause","String","Integer","Double","QIdent","CustomOperator","QualifiedIdentifier","Program","ListImport","Import","Declaration","DataCase","ListType","ListDataCase","Arg","TypeQualifier","ListTypeQualifier","ListDeclaration","ListArg","Type2","Type","ListType2","Type1","Exp1","Exp2","Exp5","Exp3","Exp4","ListExp","Exp","Case","ListCase","ListQIdent","DoClause","ListDoClause","'('","'()'","')'","'*'","'+'","','","'-'","'->'","'.'","'/'","':'","';'","'<-'","'='","'???'","'['","'\\\\'","']'","'case'","'data'","'def'","'defop'","'do'","'else'","'if'","'import'","'of'","'then'","'val'","'{'","'|'","'}'","L_quoted","L_integ","L_doubl","L_QIdent","L_CustomOperator","%eof"]
        bit_start = st * 103
        bit_end = (st + 1) * 103
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..102]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (101) = happyShift action_56
action_0 (35) = happyGoto action_63
action_0 (37) = happyGoto action_108
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (38) = happyGoto action_106
action_1 (39) = happyGoto action_107
action_1 _ = happyReduce_37

action_2 (39) = happyGoto action_105
action_2 _ = happyReduce_37

action_3 (91) = happyShift action_104
action_3 (40) = happyGoto action_103
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (85) = happyShift action_99
action_4 (86) = happyShift action_100
action_4 (87) = happyShift action_101
action_4 (94) = happyShift action_102
action_4 (41) = happyGoto action_98
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (101) = happyShift action_56
action_5 (35) = happyGoto action_92
action_5 (42) = happyGoto action_97
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (66) = happyShift action_75
action_6 (67) = happyShift action_76
action_6 (101) = happyShift action_56
action_6 (35) = happyGoto action_63
action_6 (37) = happyGoto action_79
action_6 (43) = happyGoto action_95
action_6 (50) = happyGoto action_80
action_6 (51) = happyGoto action_96
action_6 (53) = happyGoto action_82
action_6 _ = happyReduce_47

action_7 (101) = happyShift action_56
action_7 (35) = happyGoto action_92
action_7 (42) = happyGoto action_93
action_7 (44) = happyGoto action_94
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (101) = happyShift action_56
action_8 (35) = happyGoto action_84
action_8 (45) = happyGoto action_91
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (81) = happyShift action_90
action_9 (46) = happyGoto action_89
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (47) = happyGoto action_88
action_10 _ = happyReduce_55

action_11 (48) = happyGoto action_87
action_11 _ = happyReduce_57

action_12 (101) = happyShift action_56
action_12 (35) = happyGoto action_84
action_12 (45) = happyGoto action_85
action_12 (49) = happyGoto action_86
action_12 _ = happyReduce_59

action_13 (66) = happyShift action_75
action_13 (67) = happyShift action_76
action_13 (101) = happyShift action_56
action_13 (35) = happyGoto action_63
action_13 (37) = happyGoto action_72
action_13 (50) = happyGoto action_83
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (66) = happyShift action_75
action_14 (67) = happyShift action_76
action_14 (101) = happyShift action_56
action_14 (35) = happyGoto action_63
action_14 (37) = happyGoto action_79
action_14 (50) = happyGoto action_80
action_14 (51) = happyGoto action_81
action_14 (53) = happyGoto action_82
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (66) = happyShift action_75
action_15 (67) = happyShift action_76
action_15 (101) = happyShift action_56
action_15 (35) = happyGoto action_63
action_15 (37) = happyGoto action_72
action_15 (50) = happyGoto action_77
action_15 (52) = happyGoto action_78
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (66) = happyShift action_75
action_16 (67) = happyShift action_76
action_16 (101) = happyShift action_56
action_16 (35) = happyGoto action_63
action_16 (37) = happyGoto action_72
action_16 (50) = happyGoto action_73
action_16 (53) = happyGoto action_74
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (66) = happyShift action_44
action_17 (67) = happyShift action_45
action_17 (72) = happyShift action_46
action_17 (80) = happyShift action_47
action_17 (81) = happyShift action_48
action_17 (82) = happyShift action_49
action_17 (84) = happyShift action_50
action_17 (88) = happyShift action_51
action_17 (90) = happyShift action_52
action_17 (95) = happyShift action_53
action_17 (98) = happyShift action_30
action_17 (99) = happyShift action_54
action_17 (100) = happyShift action_55
action_17 (101) = happyShift action_56
action_17 (32) = happyGoto action_31
action_17 (33) = happyGoto action_32
action_17 (34) = happyGoto action_33
action_17 (35) = happyGoto action_63
action_17 (37) = happyGoto action_35
action_17 (54) = happyGoto action_71
action_17 (55) = happyGoto action_37
action_17 (56) = happyGoto action_38
action_17 (57) = happyGoto action_39
action_17 (58) = happyGoto action_40
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (66) = happyShift action_44
action_18 (67) = happyShift action_45
action_18 (72) = happyShift action_46
action_18 (80) = happyShift action_47
action_18 (81) = happyShift action_48
action_18 (84) = happyShift action_50
action_18 (88) = happyShift action_51
action_18 (90) = happyShift action_52
action_18 (95) = happyShift action_53
action_18 (98) = happyShift action_30
action_18 (99) = happyShift action_54
action_18 (100) = happyShift action_55
action_18 (101) = happyShift action_56
action_18 (32) = happyGoto action_31
action_18 (33) = happyGoto action_32
action_18 (34) = happyGoto action_33
action_18 (35) = happyGoto action_63
action_18 (37) = happyGoto action_35
action_18 (55) = happyGoto action_70
action_18 (56) = happyGoto action_38
action_18 (57) = happyGoto action_39
action_18 (58) = happyGoto action_40
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (66) = happyShift action_44
action_19 (67) = happyShift action_45
action_19 (80) = happyShift action_47
action_19 (81) = happyShift action_48
action_19 (84) = happyShift action_50
action_19 (88) = happyShift action_51
action_19 (90) = happyShift action_52
action_19 (95) = happyShift action_53
action_19 (98) = happyShift action_30
action_19 (99) = happyShift action_54
action_19 (100) = happyShift action_55
action_19 (101) = happyShift action_56
action_19 (32) = happyGoto action_31
action_19 (33) = happyGoto action_32
action_19 (34) = happyGoto action_33
action_19 (35) = happyGoto action_63
action_19 (37) = happyGoto action_35
action_19 (56) = happyGoto action_69
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (66) = happyShift action_44
action_20 (67) = happyShift action_45
action_20 (80) = happyShift action_47
action_20 (81) = happyShift action_48
action_20 (84) = happyShift action_50
action_20 (88) = happyShift action_51
action_20 (90) = happyShift action_52
action_20 (95) = happyShift action_53
action_20 (98) = happyShift action_30
action_20 (99) = happyShift action_54
action_20 (100) = happyShift action_55
action_20 (101) = happyShift action_56
action_20 (32) = happyGoto action_31
action_20 (33) = happyGoto action_32
action_20 (34) = happyGoto action_33
action_20 (35) = happyGoto action_63
action_20 (37) = happyGoto action_35
action_20 (56) = happyGoto action_38
action_20 (57) = happyGoto action_68
action_20 (58) = happyGoto action_40
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (66) = happyShift action_44
action_21 (67) = happyShift action_45
action_21 (80) = happyShift action_47
action_21 (81) = happyShift action_48
action_21 (84) = happyShift action_50
action_21 (88) = happyShift action_51
action_21 (90) = happyShift action_52
action_21 (95) = happyShift action_53
action_21 (98) = happyShift action_30
action_21 (99) = happyShift action_54
action_21 (100) = happyShift action_55
action_21 (101) = happyShift action_56
action_21 (32) = happyGoto action_31
action_21 (33) = happyGoto action_32
action_21 (34) = happyGoto action_33
action_21 (35) = happyGoto action_63
action_21 (37) = happyGoto action_35
action_21 (56) = happyGoto action_38
action_21 (58) = happyGoto action_67
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (66) = happyShift action_44
action_22 (67) = happyShift action_45
action_22 (72) = happyShift action_46
action_22 (80) = happyShift action_47
action_22 (81) = happyShift action_48
action_22 (82) = happyShift action_49
action_22 (84) = happyShift action_50
action_22 (88) = happyShift action_51
action_22 (90) = happyShift action_52
action_22 (95) = happyShift action_53
action_22 (98) = happyShift action_30
action_22 (99) = happyShift action_54
action_22 (100) = happyShift action_55
action_22 (101) = happyShift action_56
action_22 (32) = happyGoto action_31
action_22 (33) = happyGoto action_32
action_22 (34) = happyGoto action_33
action_22 (35) = happyGoto action_63
action_22 (37) = happyGoto action_35
action_22 (54) = happyGoto action_36
action_22 (55) = happyGoto action_37
action_22 (56) = happyGoto action_38
action_22 (57) = happyGoto action_39
action_22 (58) = happyGoto action_40
action_22 (59) = happyGoto action_65
action_22 (60) = happyGoto action_66
action_22 _ = happyReduce_97

action_23 (66) = happyShift action_44
action_23 (67) = happyShift action_45
action_23 (72) = happyShift action_46
action_23 (80) = happyShift action_47
action_23 (81) = happyShift action_48
action_23 (82) = happyShift action_49
action_23 (84) = happyShift action_50
action_23 (88) = happyShift action_51
action_23 (90) = happyShift action_52
action_23 (95) = happyShift action_53
action_23 (98) = happyShift action_30
action_23 (99) = happyShift action_54
action_23 (100) = happyShift action_55
action_23 (101) = happyShift action_56
action_23 (32) = happyGoto action_31
action_23 (33) = happyGoto action_32
action_23 (34) = happyGoto action_33
action_23 (35) = happyGoto action_63
action_23 (37) = happyGoto action_35
action_23 (54) = happyGoto action_36
action_23 (55) = happyGoto action_37
action_23 (56) = happyGoto action_38
action_23 (57) = happyGoto action_39
action_23 (58) = happyGoto action_40
action_23 (60) = happyGoto action_64
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (96) = happyShift action_62
action_24 (61) = happyGoto action_61
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (62) = happyGoto action_60
action_25 _ = happyReduce_102

action_26 (101) = happyShift action_56
action_26 (35) = happyGoto action_58
action_26 (63) = happyGoto action_59
action_26 _ = happyReduce_104

action_27 (66) = happyShift action_44
action_27 (67) = happyShift action_45
action_27 (72) = happyShift action_46
action_27 (80) = happyShift action_47
action_27 (81) = happyShift action_48
action_27 (82) = happyShift action_49
action_27 (84) = happyShift action_50
action_27 (88) = happyShift action_51
action_27 (90) = happyShift action_52
action_27 (95) = happyShift action_53
action_27 (98) = happyShift action_30
action_27 (99) = happyShift action_54
action_27 (100) = happyShift action_55
action_27 (101) = happyShift action_56
action_27 (32) = happyGoto action_31
action_27 (33) = happyGoto action_32
action_27 (34) = happyGoto action_33
action_27 (35) = happyGoto action_34
action_27 (37) = happyGoto action_35
action_27 (54) = happyGoto action_36
action_27 (55) = happyGoto action_37
action_27 (56) = happyGoto action_38
action_27 (57) = happyGoto action_39
action_27 (58) = happyGoto action_40
action_27 (60) = happyGoto action_41
action_27 (64) = happyGoto action_57
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (66) = happyShift action_44
action_28 (67) = happyShift action_45
action_28 (72) = happyShift action_46
action_28 (80) = happyShift action_47
action_28 (81) = happyShift action_48
action_28 (82) = happyShift action_49
action_28 (84) = happyShift action_50
action_28 (88) = happyShift action_51
action_28 (90) = happyShift action_52
action_28 (95) = happyShift action_53
action_28 (98) = happyShift action_30
action_28 (99) = happyShift action_54
action_28 (100) = happyShift action_55
action_28 (101) = happyShift action_56
action_28 (32) = happyGoto action_31
action_28 (33) = happyGoto action_32
action_28 (34) = happyGoto action_33
action_28 (35) = happyGoto action_34
action_28 (37) = happyGoto action_35
action_28 (54) = happyGoto action_36
action_28 (55) = happyGoto action_37
action_28 (56) = happyGoto action_38
action_28 (57) = happyGoto action_39
action_28 (58) = happyGoto action_40
action_28 (60) = happyGoto action_41
action_28 (64) = happyGoto action_42
action_28 (65) = happyGoto action_43
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (98) = happyShift action_30
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_29

action_31 _ = happyReduce_80

action_32 _ = happyReduce_81

action_33 _ = happyReduce_82

action_34 (74) = happyShift action_135
action_34 (78) = happyShift action_150
action_34 _ = happyReduce_35

action_35 _ = happyReduce_79

action_36 _ = happyReduce_100

action_37 (66) = happyShift action_44
action_37 (67) = happyShift action_45
action_37 (80) = happyShift action_47
action_37 (81) = happyShift action_48
action_37 (84) = happyShift action_50
action_37 (88) = happyShift action_51
action_37 (90) = happyShift action_52
action_37 (95) = happyShift action_53
action_37 (98) = happyShift action_30
action_37 (99) = happyShift action_54
action_37 (100) = happyShift action_55
action_37 (101) = happyShift action_56
action_37 (102) = happyShift action_114
action_37 (32) = happyGoto action_31
action_37 (33) = happyGoto action_32
action_37 (34) = happyGoto action_33
action_37 (35) = happyGoto action_63
action_37 (36) = happyGoto action_149
action_37 (37) = happyGoto action_35
action_37 (56) = happyGoto action_38
action_37 (57) = happyGoto action_129
action_37 (58) = happyGoto action_40
action_37 _ = happyReduce_73

action_38 _ = happyReduce_96

action_39 (70) = happyShift action_130
action_39 (72) = happyShift action_131
action_39 _ = happyReduce_77

action_40 (69) = happyShift action_132
action_40 (75) = happyShift action_133
action_40 _ = happyReduce_93

action_41 _ = happyReduce_106

action_42 (77) = happyShift action_148
action_42 _ = happyReduce_108

action_43 (103) = happyAccept
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (66) = happyShift action_44
action_44 (67) = happyShift action_45
action_44 (72) = happyShift action_46
action_44 (80) = happyShift action_47
action_44 (81) = happyShift action_48
action_44 (82) = happyShift action_49
action_44 (84) = happyShift action_50
action_44 (88) = happyShift action_51
action_44 (90) = happyShift action_52
action_44 (95) = happyShift action_53
action_44 (98) = happyShift action_30
action_44 (99) = happyShift action_54
action_44 (100) = happyShift action_55
action_44 (101) = happyShift action_56
action_44 (32) = happyGoto action_31
action_44 (33) = happyGoto action_32
action_44 (34) = happyGoto action_33
action_44 (35) = happyGoto action_63
action_44 (37) = happyGoto action_35
action_44 (54) = happyGoto action_36
action_44 (55) = happyGoto action_37
action_44 (56) = happyGoto action_38
action_44 (57) = happyGoto action_39
action_44 (58) = happyGoto action_40
action_44 (60) = happyGoto action_147
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_84

action_46 (99) = happyShift action_54
action_46 (100) = happyShift action_55
action_46 (33) = happyGoto action_145
action_46 (34) = happyGoto action_146
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_83

action_48 (66) = happyShift action_44
action_48 (67) = happyShift action_45
action_48 (72) = happyShift action_46
action_48 (80) = happyShift action_47
action_48 (81) = happyShift action_48
action_48 (82) = happyShift action_49
action_48 (84) = happyShift action_50
action_48 (88) = happyShift action_51
action_48 (90) = happyShift action_52
action_48 (95) = happyShift action_53
action_48 (98) = happyShift action_30
action_48 (99) = happyShift action_54
action_48 (100) = happyShift action_55
action_48 (101) = happyShift action_56
action_48 (32) = happyGoto action_31
action_48 (33) = happyGoto action_32
action_48 (34) = happyGoto action_33
action_48 (35) = happyGoto action_63
action_48 (37) = happyGoto action_35
action_48 (54) = happyGoto action_36
action_48 (55) = happyGoto action_37
action_48 (56) = happyGoto action_38
action_48 (57) = happyGoto action_39
action_48 (58) = happyGoto action_40
action_48 (59) = happyGoto action_144
action_48 (60) = happyGoto action_66
action_48 _ = happyReduce_97

action_49 (101) = happyShift action_56
action_49 (35) = happyGoto action_143
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (66) = happyShift action_44
action_50 (67) = happyShift action_45
action_50 (72) = happyShift action_46
action_50 (80) = happyShift action_47
action_50 (81) = happyShift action_48
action_50 (82) = happyShift action_49
action_50 (84) = happyShift action_50
action_50 (88) = happyShift action_51
action_50 (90) = happyShift action_52
action_50 (95) = happyShift action_53
action_50 (98) = happyShift action_30
action_50 (99) = happyShift action_54
action_50 (100) = happyShift action_55
action_50 (101) = happyShift action_56
action_50 (32) = happyGoto action_31
action_50 (33) = happyGoto action_32
action_50 (34) = happyGoto action_33
action_50 (35) = happyGoto action_63
action_50 (37) = happyGoto action_35
action_50 (54) = happyGoto action_36
action_50 (55) = happyGoto action_37
action_50 (56) = happyGoto action_38
action_50 (57) = happyGoto action_39
action_50 (58) = happyGoto action_40
action_50 (60) = happyGoto action_142
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (95) = happyShift action_141
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (66) = happyShift action_44
action_52 (67) = happyShift action_45
action_52 (80) = happyShift action_47
action_52 (81) = happyShift action_48
action_52 (84) = happyShift action_50
action_52 (88) = happyShift action_51
action_52 (90) = happyShift action_52
action_52 (95) = happyShift action_53
action_52 (98) = happyShift action_30
action_52 (99) = happyShift action_54
action_52 (100) = happyShift action_55
action_52 (101) = happyShift action_56
action_52 (32) = happyGoto action_31
action_52 (33) = happyGoto action_32
action_52 (34) = happyGoto action_33
action_52 (35) = happyGoto action_63
action_52 (37) = happyGoto action_35
action_52 (56) = happyGoto action_38
action_52 (58) = happyGoto action_140
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (48) = happyGoto action_139
action_53 _ = happyReduce_57

action_54 _ = happyReduce_30

action_55 _ = happyReduce_31

action_56 _ = happyReduce_32

action_57 (103) = happyAccept
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (101) = happyShift action_56
action_58 (35) = happyGoto action_58
action_58 (63) = happyGoto action_138
action_58 _ = happyReduce_104

action_59 (103) = happyAccept
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (96) = happyShift action_62
action_60 (103) = happyAccept
action_60 (61) = happyGoto action_137
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (103) = happyAccept
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (101) = happyShift action_56
action_62 (35) = happyGoto action_63
action_62 (37) = happyGoto action_136
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (74) = happyShift action_135
action_63 _ = happyReduce_35

action_64 (103) = happyAccept
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (103) = happyAccept
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (71) = happyShift action_134
action_66 _ = happyReduce_98

action_67 (69) = happyShift action_132
action_67 (75) = happyShift action_133
action_67 (103) = happyAccept
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (70) = happyShift action_130
action_68 (72) = happyShift action_131
action_68 (103) = happyAccept
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (103) = happyAccept
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (66) = happyShift action_44
action_70 (67) = happyShift action_45
action_70 (80) = happyShift action_47
action_70 (81) = happyShift action_48
action_70 (84) = happyShift action_50
action_70 (88) = happyShift action_51
action_70 (90) = happyShift action_52
action_70 (95) = happyShift action_53
action_70 (98) = happyShift action_30
action_70 (99) = happyShift action_54
action_70 (100) = happyShift action_55
action_70 (101) = happyShift action_56
action_70 (103) = happyAccept
action_70 (32) = happyGoto action_31
action_70 (33) = happyGoto action_32
action_70 (34) = happyGoto action_33
action_70 (35) = happyGoto action_63
action_70 (37) = happyGoto action_35
action_70 (56) = happyGoto action_38
action_70 (57) = happyGoto action_129
action_70 (58) = happyGoto action_40
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (103) = happyAccept
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_62

action_73 _ = happyReduce_70

action_74 (103) = happyAccept
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (66) = happyShift action_75
action_75 (67) = happyShift action_76
action_75 (101) = happyShift action_56
action_75 (35) = happyGoto action_63
action_75 (37) = happyGoto action_79
action_75 (50) = happyGoto action_80
action_75 (51) = happyGoto action_128
action_75 (53) = happyGoto action_82
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_63

action_77 (66) = happyShift action_75
action_77 (67) = happyShift action_76
action_77 (101) = happyShift action_56
action_77 (35) = happyGoto action_63
action_77 (37) = happyGoto action_72
action_77 (50) = happyGoto action_77
action_77 (52) = happyGoto action_127
action_77 _ = happyReduce_68

action_78 (103) = happyAccept
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (66) = happyShift action_75
action_79 (67) = happyShift action_76
action_79 (101) = happyShift action_56
action_79 (35) = happyGoto action_63
action_79 (37) = happyGoto action_72
action_79 (50) = happyGoto action_77
action_79 (52) = happyGoto action_126
action_79 _ = happyReduce_62

action_80 (73) = happyShift action_125
action_80 _ = happyReduce_70

action_81 (103) = happyAccept
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_67

action_83 (103) = happyAccept
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (76) = happyShift action_124
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (71) = happyShift action_123
action_85 _ = happyReduce_60

action_86 (103) = happyAccept
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (85) = happyShift action_99
action_87 (86) = happyShift action_100
action_87 (87) = happyShift action_101
action_87 (94) = happyShift action_102
action_87 (103) = happyAccept
action_87 (41) = happyGoto action_122
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (81) = happyShift action_90
action_88 (103) = happyAccept
action_88 (46) = happyGoto action_121
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (103) = happyAccept
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (101) = happyShift action_56
action_90 (35) = happyGoto action_120
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (103) = happyAccept
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (66) = happyShift action_75
action_92 (67) = happyShift action_76
action_92 (101) = happyShift action_56
action_92 (35) = happyGoto action_63
action_92 (37) = happyGoto action_79
action_92 (43) = happyGoto action_119
action_92 (50) = happyGoto action_80
action_92 (51) = happyGoto action_96
action_92 (53) = happyGoto action_82
action_92 _ = happyReduce_47

action_93 (96) = happyShift action_118
action_93 _ = happyReduce_50

action_94 (103) = happyAccept
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (103) = happyAccept
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (69) = happyShift action_117
action_96 _ = happyReduce_48

action_97 (103) = happyAccept
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (103) = happyAccept
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (101) = happyShift action_56
action_99 (35) = happyGoto action_116
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (101) = happyShift action_56
action_100 (35) = happyGoto action_115
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (102) = happyShift action_114
action_101 (36) = happyGoto action_113
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (101) = happyShift action_56
action_102 (35) = happyGoto action_112
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (103) = happyAccept
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (101) = happyShift action_56
action_104 (35) = happyGoto action_111
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (91) = happyShift action_104
action_105 (103) = happyAccept
action_105 (40) = happyGoto action_109
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (103) = happyAccept
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (91) = happyShift action_104
action_107 (40) = happyGoto action_109
action_107 (48) = happyGoto action_110
action_107 _ = happyReduce_57

action_108 (103) = happyAccept
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (77) = happyShift action_182
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (85) = happyShift action_99
action_110 (86) = happyShift action_100
action_110 (87) = happyShift action_101
action_110 (94) = happyShift action_102
action_110 (41) = happyGoto action_122
action_110 _ = happyReduce_36

action_111 _ = happyReduce_39

action_112 (76) = happyShift action_180
action_112 (79) = happyShift action_181
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (47) = happyGoto action_179
action_113 _ = happyReduce_55

action_114 _ = happyReduce_33

action_115 (47) = happyGoto action_178
action_115 _ = happyReduce_55

action_116 (101) = happyShift action_56
action_116 (35) = happyGoto action_58
action_116 (63) = happyGoto action_177
action_116 _ = happyReduce_104

action_117 (66) = happyShift action_75
action_117 (67) = happyShift action_76
action_117 (101) = happyShift action_56
action_117 (35) = happyGoto action_63
action_117 (37) = happyGoto action_79
action_117 (43) = happyGoto action_176
action_117 (50) = happyGoto action_80
action_117 (51) = happyGoto action_96
action_117 (53) = happyGoto action_82
action_117 _ = happyReduce_47

action_118 (101) = happyShift action_56
action_118 (35) = happyGoto action_92
action_118 (42) = happyGoto action_93
action_118 (44) = happyGoto action_175
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_46

action_120 (83) = happyShift action_174
action_120 _ = happyFail (happyExpListPerState 120)

action_121 _ = happyReduce_56

action_122 (77) = happyShift action_173
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (101) = happyShift action_56
action_123 (35) = happyGoto action_84
action_123 (45) = happyGoto action_85
action_123 (49) = happyGoto action_172
action_123 _ = happyReduce_59

action_124 (66) = happyShift action_75
action_124 (67) = happyShift action_76
action_124 (101) = happyShift action_56
action_124 (35) = happyGoto action_63
action_124 (37) = happyGoto action_79
action_124 (50) = happyGoto action_80
action_124 (51) = happyGoto action_171
action_124 (53) = happyGoto action_82
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (66) = happyShift action_75
action_125 (67) = happyShift action_76
action_125 (101) = happyShift action_56
action_125 (35) = happyGoto action_63
action_125 (37) = happyGoto action_79
action_125 (50) = happyGoto action_80
action_125 (51) = happyGoto action_170
action_125 (53) = happyGoto action_82
action_125 _ = happyFail (happyExpListPerState 125)

action_126 _ = happyReduce_66

action_127 _ = happyReduce_69

action_128 (68) = happyShift action_169
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (70) = happyShift action_130
action_129 (72) = happyShift action_131
action_129 _ = happyReduce_74

action_130 (66) = happyShift action_44
action_130 (67) = happyShift action_45
action_130 (80) = happyShift action_47
action_130 (81) = happyShift action_48
action_130 (84) = happyShift action_50
action_130 (88) = happyShift action_51
action_130 (90) = happyShift action_52
action_130 (95) = happyShift action_53
action_130 (98) = happyShift action_30
action_130 (99) = happyShift action_54
action_130 (100) = happyShift action_55
action_130 (101) = happyShift action_56
action_130 (32) = happyGoto action_31
action_130 (33) = happyGoto action_32
action_130 (34) = happyGoto action_33
action_130 (35) = happyGoto action_63
action_130 (37) = happyGoto action_35
action_130 (56) = happyGoto action_38
action_130 (58) = happyGoto action_168
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (66) = happyShift action_44
action_131 (67) = happyShift action_45
action_131 (80) = happyShift action_47
action_131 (81) = happyShift action_48
action_131 (84) = happyShift action_50
action_131 (88) = happyShift action_51
action_131 (90) = happyShift action_52
action_131 (95) = happyShift action_53
action_131 (98) = happyShift action_30
action_131 (99) = happyShift action_54
action_131 (100) = happyShift action_55
action_131 (101) = happyShift action_56
action_131 (32) = happyGoto action_31
action_131 (33) = happyGoto action_32
action_131 (34) = happyGoto action_33
action_131 (35) = happyGoto action_63
action_131 (37) = happyGoto action_35
action_131 (56) = happyGoto action_38
action_131 (58) = happyGoto action_167
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (66) = happyShift action_44
action_132 (67) = happyShift action_45
action_132 (80) = happyShift action_47
action_132 (81) = happyShift action_48
action_132 (84) = happyShift action_50
action_132 (88) = happyShift action_51
action_132 (90) = happyShift action_52
action_132 (95) = happyShift action_53
action_132 (98) = happyShift action_30
action_132 (99) = happyShift action_54
action_132 (100) = happyShift action_55
action_132 (101) = happyShift action_56
action_132 (32) = happyGoto action_31
action_132 (33) = happyGoto action_32
action_132 (34) = happyGoto action_33
action_132 (35) = happyGoto action_63
action_132 (37) = happyGoto action_35
action_132 (56) = happyGoto action_166
action_132 _ = happyFail (happyExpListPerState 132)

action_133 (66) = happyShift action_44
action_133 (67) = happyShift action_45
action_133 (80) = happyShift action_47
action_133 (81) = happyShift action_48
action_133 (84) = happyShift action_50
action_133 (88) = happyShift action_51
action_133 (90) = happyShift action_52
action_133 (95) = happyShift action_53
action_133 (98) = happyShift action_30
action_133 (99) = happyShift action_54
action_133 (100) = happyShift action_55
action_133 (101) = happyShift action_56
action_133 (32) = happyGoto action_31
action_133 (33) = happyGoto action_32
action_133 (34) = happyGoto action_33
action_133 (35) = happyGoto action_63
action_133 (37) = happyGoto action_35
action_133 (56) = happyGoto action_165
action_133 _ = happyFail (happyExpListPerState 133)

action_134 (66) = happyShift action_44
action_134 (67) = happyShift action_45
action_134 (72) = happyShift action_46
action_134 (80) = happyShift action_47
action_134 (81) = happyShift action_48
action_134 (82) = happyShift action_49
action_134 (84) = happyShift action_50
action_134 (88) = happyShift action_51
action_134 (90) = happyShift action_52
action_134 (95) = happyShift action_53
action_134 (98) = happyShift action_30
action_134 (99) = happyShift action_54
action_134 (100) = happyShift action_55
action_134 (101) = happyShift action_56
action_134 (32) = happyGoto action_31
action_134 (33) = happyGoto action_32
action_134 (34) = happyGoto action_33
action_134 (35) = happyGoto action_63
action_134 (37) = happyGoto action_35
action_134 (54) = happyGoto action_36
action_134 (55) = happyGoto action_37
action_134 (56) = happyGoto action_38
action_134 (57) = happyGoto action_39
action_134 (58) = happyGoto action_40
action_134 (59) = happyGoto action_164
action_134 (60) = happyGoto action_66
action_134 _ = happyReduce_97

action_135 (101) = happyShift action_56
action_135 (35) = happyGoto action_163
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (101) = happyShift action_56
action_136 (35) = happyGoto action_58
action_136 (63) = happyGoto action_162
action_136 _ = happyReduce_104

action_137 _ = happyReduce_103

action_138 _ = happyReduce_105

action_139 (66) = happyShift action_44
action_139 (67) = happyShift action_45
action_139 (72) = happyShift action_46
action_139 (80) = happyShift action_47
action_139 (81) = happyShift action_48
action_139 (82) = happyShift action_49
action_139 (84) = happyShift action_50
action_139 (85) = happyShift action_99
action_139 (86) = happyShift action_100
action_139 (87) = happyShift action_101
action_139 (88) = happyShift action_51
action_139 (90) = happyShift action_52
action_139 (94) = happyShift action_102
action_139 (95) = happyShift action_53
action_139 (98) = happyShift action_30
action_139 (99) = happyShift action_54
action_139 (100) = happyShift action_55
action_139 (101) = happyShift action_56
action_139 (32) = happyGoto action_31
action_139 (33) = happyGoto action_32
action_139 (34) = happyGoto action_33
action_139 (35) = happyGoto action_63
action_139 (37) = happyGoto action_35
action_139 (41) = happyGoto action_122
action_139 (54) = happyGoto action_36
action_139 (55) = happyGoto action_37
action_139 (56) = happyGoto action_38
action_139 (57) = happyGoto action_39
action_139 (58) = happyGoto action_40
action_139 (60) = happyGoto action_161
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (69) = happyShift action_132
action_140 (75) = happyShift action_133
action_140 (93) = happyShift action_160
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (66) = happyShift action_44
action_141 (67) = happyShift action_45
action_141 (72) = happyShift action_46
action_141 (80) = happyShift action_47
action_141 (81) = happyShift action_48
action_141 (82) = happyShift action_49
action_141 (84) = happyShift action_50
action_141 (88) = happyShift action_51
action_141 (90) = happyShift action_52
action_141 (95) = happyShift action_53
action_141 (98) = happyShift action_30
action_141 (99) = happyShift action_54
action_141 (100) = happyShift action_55
action_141 (101) = happyShift action_56
action_141 (32) = happyGoto action_31
action_141 (33) = happyGoto action_32
action_141 (34) = happyGoto action_33
action_141 (35) = happyGoto action_34
action_141 (37) = happyGoto action_35
action_141 (54) = happyGoto action_36
action_141 (55) = happyGoto action_37
action_141 (56) = happyGoto action_38
action_141 (57) = happyGoto action_39
action_141 (58) = happyGoto action_40
action_141 (60) = happyGoto action_41
action_141 (64) = happyGoto action_42
action_141 (65) = happyGoto action_159
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (92) = happyShift action_158
action_142 _ = happyFail (happyExpListPerState 142)

action_143 (73) = happyShift action_157
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (83) = happyShift action_156
action_144 _ = happyFail (happyExpListPerState 144)

action_145 _ = happyReduce_75

action_146 _ = happyReduce_76

action_147 (68) = happyShift action_154
action_147 (71) = happyShift action_155
action_147 _ = happyFail (happyExpListPerState 147)

action_148 (66) = happyShift action_44
action_148 (67) = happyShift action_45
action_148 (72) = happyShift action_46
action_148 (80) = happyShift action_47
action_148 (81) = happyShift action_48
action_148 (82) = happyShift action_49
action_148 (84) = happyShift action_50
action_148 (88) = happyShift action_51
action_148 (90) = happyShift action_52
action_148 (95) = happyShift action_53
action_148 (98) = happyShift action_30
action_148 (99) = happyShift action_54
action_148 (100) = happyShift action_55
action_148 (101) = happyShift action_56
action_148 (32) = happyGoto action_31
action_148 (33) = happyGoto action_32
action_148 (34) = happyGoto action_33
action_148 (35) = happyGoto action_34
action_148 (37) = happyGoto action_35
action_148 (54) = happyGoto action_36
action_148 (55) = happyGoto action_37
action_148 (56) = happyGoto action_38
action_148 (57) = happyGoto action_39
action_148 (58) = happyGoto action_40
action_148 (60) = happyGoto action_41
action_148 (64) = happyGoto action_42
action_148 (65) = happyGoto action_153
action_148 _ = happyFail (happyExpListPerState 148)

action_149 (66) = happyShift action_44
action_149 (67) = happyShift action_45
action_149 (72) = happyShift action_46
action_149 (80) = happyShift action_47
action_149 (81) = happyShift action_48
action_149 (82) = happyShift action_49
action_149 (84) = happyShift action_50
action_149 (88) = happyShift action_51
action_149 (90) = happyShift action_52
action_149 (95) = happyShift action_53
action_149 (98) = happyShift action_30
action_149 (99) = happyShift action_54
action_149 (100) = happyShift action_55
action_149 (101) = happyShift action_56
action_149 (32) = happyGoto action_31
action_149 (33) = happyGoto action_32
action_149 (34) = happyGoto action_33
action_149 (35) = happyGoto action_63
action_149 (37) = happyGoto action_35
action_149 (54) = happyGoto action_152
action_149 (55) = happyGoto action_37
action_149 (56) = happyGoto action_38
action_149 (57) = happyGoto action_39
action_149 (58) = happyGoto action_40
action_149 _ = happyFail (happyExpListPerState 149)

action_150 (66) = happyShift action_44
action_150 (67) = happyShift action_45
action_150 (72) = happyShift action_46
action_150 (80) = happyShift action_47
action_150 (81) = happyShift action_48
action_150 (82) = happyShift action_49
action_150 (84) = happyShift action_50
action_150 (88) = happyShift action_51
action_150 (90) = happyShift action_52
action_150 (95) = happyShift action_53
action_150 (98) = happyShift action_30
action_150 (99) = happyShift action_54
action_150 (100) = happyShift action_55
action_150 (101) = happyShift action_56
action_150 (32) = happyGoto action_31
action_150 (33) = happyGoto action_32
action_150 (34) = happyGoto action_33
action_150 (35) = happyGoto action_63
action_150 (37) = happyGoto action_35
action_150 (54) = happyGoto action_36
action_150 (55) = happyGoto action_37
action_150 (56) = happyGoto action_38
action_150 (57) = happyGoto action_39
action_150 (58) = happyGoto action_40
action_150 (60) = happyGoto action_151
action_150 _ = happyFail (happyExpListPerState 150)

action_151 _ = happyReduce_107

action_152 _ = happyReduce_71

action_153 _ = happyReduce_109

action_154 _ = happyReduce_90

action_155 (66) = happyShift action_44
action_155 (67) = happyShift action_45
action_155 (72) = happyShift action_46
action_155 (80) = happyShift action_47
action_155 (81) = happyShift action_48
action_155 (82) = happyShift action_49
action_155 (84) = happyShift action_50
action_155 (88) = happyShift action_51
action_155 (90) = happyShift action_52
action_155 (95) = happyShift action_53
action_155 (98) = happyShift action_30
action_155 (99) = happyShift action_54
action_155 (100) = happyShift action_55
action_155 (101) = happyShift action_56
action_155 (32) = happyGoto action_31
action_155 (33) = happyGoto action_32
action_155 (34) = happyGoto action_33
action_155 (35) = happyGoto action_63
action_155 (37) = happyGoto action_35
action_155 (54) = happyGoto action_36
action_155 (55) = happyGoto action_37
action_155 (56) = happyGoto action_38
action_155 (57) = happyGoto action_39
action_155 (58) = happyGoto action_40
action_155 (59) = happyGoto action_196
action_155 (60) = happyGoto action_66
action_155 _ = happyReduce_97

action_156 _ = happyReduce_86

action_157 (66) = happyShift action_44
action_157 (67) = happyShift action_45
action_157 (72) = happyShift action_46
action_157 (80) = happyShift action_47
action_157 (81) = happyShift action_48
action_157 (82) = happyShift action_49
action_157 (84) = happyShift action_50
action_157 (88) = happyShift action_51
action_157 (90) = happyShift action_52
action_157 (95) = happyShift action_53
action_157 (98) = happyShift action_30
action_157 (99) = happyShift action_54
action_157 (100) = happyShift action_55
action_157 (101) = happyShift action_56
action_157 (32) = happyGoto action_31
action_157 (33) = happyGoto action_32
action_157 (34) = happyGoto action_33
action_157 (35) = happyGoto action_63
action_157 (37) = happyGoto action_35
action_157 (54) = happyGoto action_36
action_157 (55) = happyGoto action_37
action_157 (56) = happyGoto action_38
action_157 (57) = happyGoto action_39
action_157 (58) = happyGoto action_40
action_157 (60) = happyGoto action_195
action_157 _ = happyFail (happyExpListPerState 157)

action_158 (95) = happyShift action_194
action_158 _ = happyFail (happyExpListPerState 158)

action_159 (97) = happyShift action_193
action_159 _ = happyFail (happyExpListPerState 159)

action_160 (66) = happyShift action_44
action_160 (67) = happyShift action_45
action_160 (80) = happyShift action_47
action_160 (81) = happyShift action_48
action_160 (84) = happyShift action_50
action_160 (88) = happyShift action_51
action_160 (90) = happyShift action_52
action_160 (95) = happyShift action_53
action_160 (98) = happyShift action_30
action_160 (99) = happyShift action_54
action_160 (100) = happyShift action_55
action_160 (101) = happyShift action_56
action_160 (32) = happyGoto action_31
action_160 (33) = happyGoto action_32
action_160 (34) = happyGoto action_33
action_160 (35) = happyGoto action_63
action_160 (37) = happyGoto action_35
action_160 (56) = happyGoto action_192
action_160 _ = happyFail (happyExpListPerState 160)

action_161 (97) = happyShift action_191
action_161 _ = happyFail (happyExpListPerState 161)

action_162 (73) = happyShift action_190
action_162 _ = happyFail (happyExpListPerState 162)

action_163 _ = happyReduce_34

action_164 _ = happyReduce_99

action_165 _ = happyReduce_95

action_166 _ = happyReduce_94

action_167 (69) = happyShift action_132
action_167 (75) = happyShift action_133
action_167 _ = happyReduce_92

action_168 (69) = happyShift action_132
action_168 (75) = happyShift action_133
action_168 _ = happyReduce_91

action_169 _ = happyReduce_64

action_170 _ = happyReduce_65

action_171 (79) = happyShift action_189
action_171 _ = happyReduce_52

action_172 _ = happyReduce_61

action_173 _ = happyReduce_58

action_174 _ = happyReduce_54

action_175 _ = happyReduce_51

action_176 _ = happyReduce_49

action_177 (79) = happyShift action_188
action_177 _ = happyFail (happyExpListPerState 177)

action_178 (66) = happyShift action_186
action_178 (67) = happyShift action_187
action_178 (81) = happyShift action_90
action_178 (46) = happyGoto action_121
action_178 _ = happyFail (happyExpListPerState 178)

action_179 (66) = happyShift action_185
action_179 (81) = happyShift action_90
action_179 (46) = happyGoto action_121
action_179 _ = happyFail (happyExpListPerState 179)

action_180 (66) = happyShift action_75
action_180 (67) = happyShift action_76
action_180 (101) = happyShift action_56
action_180 (35) = happyGoto action_63
action_180 (37) = happyGoto action_79
action_180 (50) = happyGoto action_80
action_180 (51) = happyGoto action_184
action_180 (53) = happyGoto action_82
action_180 _ = happyFail (happyExpListPerState 180)

action_181 (66) = happyShift action_44
action_181 (67) = happyShift action_45
action_181 (72) = happyShift action_46
action_181 (80) = happyShift action_47
action_181 (81) = happyShift action_48
action_181 (82) = happyShift action_49
action_181 (84) = happyShift action_50
action_181 (88) = happyShift action_51
action_181 (90) = happyShift action_52
action_181 (95) = happyShift action_53
action_181 (98) = happyShift action_30
action_181 (99) = happyShift action_54
action_181 (100) = happyShift action_55
action_181 (101) = happyShift action_56
action_181 (32) = happyGoto action_31
action_181 (33) = happyGoto action_32
action_181 (34) = happyGoto action_33
action_181 (35) = happyGoto action_63
action_181 (37) = happyGoto action_35
action_181 (54) = happyGoto action_36
action_181 (55) = happyGoto action_37
action_181 (56) = happyGoto action_38
action_181 (57) = happyGoto action_39
action_181 (58) = happyGoto action_40
action_181 (60) = happyGoto action_183
action_181 _ = happyFail (happyExpListPerState 181)

action_182 _ = happyReduce_38

action_183 _ = happyReduce_44

action_184 (79) = happyShift action_206
action_184 _ = happyFail (happyExpListPerState 184)

action_185 (101) = happyShift action_56
action_185 (35) = happyGoto action_84
action_185 (45) = happyGoto action_205
action_185 _ = happyFail (happyExpListPerState 185)

action_186 (101) = happyShift action_56
action_186 (35) = happyGoto action_84
action_186 (45) = happyGoto action_85
action_186 (49) = happyGoto action_204
action_186 _ = happyReduce_59

action_187 (76) = happyShift action_203
action_187 _ = happyFail (happyExpListPerState 187)

action_188 (101) = happyShift action_56
action_188 (35) = happyGoto action_92
action_188 (42) = happyGoto action_93
action_188 (44) = happyGoto action_202
action_188 _ = happyFail (happyExpListPerState 188)

action_189 (66) = happyShift action_44
action_189 (67) = happyShift action_45
action_189 (72) = happyShift action_46
action_189 (80) = happyShift action_47
action_189 (81) = happyShift action_48
action_189 (82) = happyShift action_49
action_189 (84) = happyShift action_50
action_189 (88) = happyShift action_51
action_189 (90) = happyShift action_52
action_189 (95) = happyShift action_53
action_189 (98) = happyShift action_30
action_189 (99) = happyShift action_54
action_189 (100) = happyShift action_55
action_189 (101) = happyShift action_56
action_189 (32) = happyGoto action_31
action_189 (33) = happyGoto action_32
action_189 (34) = happyGoto action_33
action_189 (35) = happyGoto action_63
action_189 (37) = happyGoto action_35
action_189 (54) = happyGoto action_36
action_189 (55) = happyGoto action_37
action_189 (56) = happyGoto action_38
action_189 (57) = happyGoto action_39
action_189 (58) = happyGoto action_40
action_189 (60) = happyGoto action_201
action_189 _ = happyFail (happyExpListPerState 189)

action_190 (66) = happyShift action_44
action_190 (67) = happyShift action_45
action_190 (72) = happyShift action_46
action_190 (80) = happyShift action_47
action_190 (81) = happyShift action_48
action_190 (82) = happyShift action_49
action_190 (84) = happyShift action_50
action_190 (88) = happyShift action_51
action_190 (90) = happyShift action_52
action_190 (95) = happyShift action_53
action_190 (98) = happyShift action_30
action_190 (99) = happyShift action_54
action_190 (100) = happyShift action_55
action_190 (101) = happyShift action_56
action_190 (32) = happyGoto action_31
action_190 (33) = happyGoto action_32
action_190 (34) = happyGoto action_33
action_190 (35) = happyGoto action_63
action_190 (37) = happyGoto action_35
action_190 (54) = happyGoto action_36
action_190 (55) = happyGoto action_37
action_190 (56) = happyGoto action_38
action_190 (57) = happyGoto action_39
action_190 (58) = happyGoto action_40
action_190 (60) = happyGoto action_200
action_190 _ = happyFail (happyExpListPerState 190)

action_191 _ = happyReduce_85

action_192 (89) = happyShift action_199
action_192 _ = happyFail (happyExpListPerState 192)

action_193 _ = happyReduce_89

action_194 (62) = happyGoto action_198
action_194 _ = happyReduce_102

action_195 _ = happyReduce_72

action_196 (68) = happyShift action_197
action_196 _ = happyFail (happyExpListPerState 196)

action_197 _ = happyReduce_87

action_198 (96) = happyShift action_62
action_198 (97) = happyShift action_212
action_198 (61) = happyGoto action_137
action_198 _ = happyFail (happyExpListPerState 198)

action_199 (66) = happyShift action_44
action_199 (67) = happyShift action_45
action_199 (80) = happyShift action_47
action_199 (81) = happyShift action_48
action_199 (84) = happyShift action_50
action_199 (88) = happyShift action_51
action_199 (90) = happyShift action_52
action_199 (95) = happyShift action_53
action_199 (98) = happyShift action_30
action_199 (99) = happyShift action_54
action_199 (100) = happyShift action_55
action_199 (101) = happyShift action_56
action_199 (32) = happyGoto action_31
action_199 (33) = happyGoto action_32
action_199 (34) = happyGoto action_33
action_199 (35) = happyGoto action_63
action_199 (37) = happyGoto action_35
action_199 (56) = happyGoto action_211
action_199 _ = happyFail (happyExpListPerState 199)

action_200 _ = happyReduce_101

action_201 _ = happyReduce_53

action_202 _ = happyReduce_45

action_203 (66) = happyShift action_75
action_203 (67) = happyShift action_76
action_203 (101) = happyShift action_56
action_203 (35) = happyGoto action_63
action_203 (37) = happyGoto action_79
action_203 (50) = happyGoto action_80
action_203 (51) = happyGoto action_210
action_203 (53) = happyGoto action_82
action_203 _ = happyFail (happyExpListPerState 203)

action_204 (68) = happyShift action_209
action_204 _ = happyFail (happyExpListPerState 204)

action_205 (71) = happyShift action_208
action_205 _ = happyFail (happyExpListPerState 205)

action_206 (66) = happyShift action_44
action_206 (67) = happyShift action_45
action_206 (72) = happyShift action_46
action_206 (80) = happyShift action_47
action_206 (81) = happyShift action_48
action_206 (82) = happyShift action_49
action_206 (84) = happyShift action_50
action_206 (88) = happyShift action_51
action_206 (90) = happyShift action_52
action_206 (95) = happyShift action_53
action_206 (98) = happyShift action_30
action_206 (99) = happyShift action_54
action_206 (100) = happyShift action_55
action_206 (101) = happyShift action_56
action_206 (32) = happyGoto action_31
action_206 (33) = happyGoto action_32
action_206 (34) = happyGoto action_33
action_206 (35) = happyGoto action_63
action_206 (37) = happyGoto action_35
action_206 (54) = happyGoto action_36
action_206 (55) = happyGoto action_37
action_206 (56) = happyGoto action_38
action_206 (57) = happyGoto action_39
action_206 (58) = happyGoto action_40
action_206 (60) = happyGoto action_207
action_206 _ = happyFail (happyExpListPerState 206)

action_207 _ = happyReduce_43

action_208 (101) = happyShift action_56
action_208 (35) = happyGoto action_84
action_208 (45) = happyGoto action_215
action_208 _ = happyFail (happyExpListPerState 208)

action_209 (76) = happyShift action_214
action_209 _ = happyFail (happyExpListPerState 209)

action_210 (79) = happyShift action_213
action_210 _ = happyFail (happyExpListPerState 210)

action_211 _ = happyReduce_78

action_212 _ = happyReduce_88

action_213 (66) = happyShift action_44
action_213 (67) = happyShift action_45
action_213 (72) = happyShift action_46
action_213 (80) = happyShift action_47
action_213 (81) = happyShift action_48
action_213 (82) = happyShift action_49
action_213 (84) = happyShift action_50
action_213 (88) = happyShift action_51
action_213 (90) = happyShift action_52
action_213 (95) = happyShift action_53
action_213 (98) = happyShift action_30
action_213 (99) = happyShift action_54
action_213 (100) = happyShift action_55
action_213 (101) = happyShift action_56
action_213 (32) = happyGoto action_31
action_213 (33) = happyGoto action_32
action_213 (34) = happyGoto action_33
action_213 (35) = happyGoto action_63
action_213 (37) = happyGoto action_35
action_213 (54) = happyGoto action_36
action_213 (55) = happyGoto action_37
action_213 (56) = happyGoto action_38
action_213 (57) = happyGoto action_39
action_213 (58) = happyGoto action_40
action_213 (60) = happyGoto action_218
action_213 _ = happyFail (happyExpListPerState 213)

action_214 (66) = happyShift action_75
action_214 (67) = happyShift action_76
action_214 (101) = happyShift action_56
action_214 (35) = happyGoto action_63
action_214 (37) = happyGoto action_79
action_214 (50) = happyGoto action_80
action_214 (51) = happyGoto action_217
action_214 (53) = happyGoto action_82
action_214 _ = happyFail (happyExpListPerState 214)

action_215 (68) = happyShift action_216
action_215 _ = happyFail (happyExpListPerState 215)

action_216 (76) = happyShift action_220
action_216 _ = happyFail (happyExpListPerState 216)

action_217 (79) = happyShift action_219
action_217 _ = happyFail (happyExpListPerState 217)

action_218 _ = happyReduce_41

action_219 (66) = happyShift action_44
action_219 (67) = happyShift action_45
action_219 (72) = happyShift action_46
action_219 (80) = happyShift action_47
action_219 (81) = happyShift action_48
action_219 (82) = happyShift action_49
action_219 (84) = happyShift action_50
action_219 (88) = happyShift action_51
action_219 (90) = happyShift action_52
action_219 (95) = happyShift action_53
action_219 (98) = happyShift action_30
action_219 (99) = happyShift action_54
action_219 (100) = happyShift action_55
action_219 (101) = happyShift action_56
action_219 (32) = happyGoto action_31
action_219 (33) = happyGoto action_32
action_219 (34) = happyGoto action_33
action_219 (35) = happyGoto action_63
action_219 (37) = happyGoto action_35
action_219 (54) = happyGoto action_36
action_219 (55) = happyGoto action_37
action_219 (56) = happyGoto action_38
action_219 (57) = happyGoto action_39
action_219 (58) = happyGoto action_40
action_219 (60) = happyGoto action_222
action_219 _ = happyFail (happyExpListPerState 219)

action_220 (66) = happyShift action_75
action_220 (67) = happyShift action_76
action_220 (101) = happyShift action_56
action_220 (35) = happyGoto action_63
action_220 (37) = happyGoto action_79
action_220 (50) = happyGoto action_80
action_220 (51) = happyGoto action_221
action_220 (53) = happyGoto action_82
action_220 _ = happyFail (happyExpListPerState 220)

action_221 (79) = happyShift action_223
action_221 _ = happyFail (happyExpListPerState 221)

action_222 _ = happyReduce_40

action_223 (66) = happyShift action_44
action_223 (67) = happyShift action_45
action_223 (72) = happyShift action_46
action_223 (80) = happyShift action_47
action_223 (81) = happyShift action_48
action_223 (82) = happyShift action_49
action_223 (84) = happyShift action_50
action_223 (88) = happyShift action_51
action_223 (90) = happyShift action_52
action_223 (95) = happyShift action_53
action_223 (98) = happyShift action_30
action_223 (99) = happyShift action_54
action_223 (100) = happyShift action_55
action_223 (101) = happyShift action_56
action_223 (32) = happyGoto action_31
action_223 (33) = happyGoto action_32
action_223 (34) = happyGoto action_33
action_223 (35) = happyGoto action_63
action_223 (37) = happyGoto action_35
action_223 (54) = happyGoto action_36
action_223 (55) = happyGoto action_37
action_223 (56) = happyGoto action_38
action_223 (57) = happyGoto action_39
action_223 (58) = happyGoto action_40
action_223 (60) = happyGoto action_224
action_223 _ = happyFail (happyExpListPerState 223)

action_224 _ = happyReduce_42

happyReduce_29 = happySpecReduce_1  32 happyReduction_29
happyReduction_29 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  33 happyReduction_30
happyReduction_30 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn33
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  34 happyReduction_31
happyReduction_31 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn34
		 ((read ( happy_var_1)) :: Double
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  35 happyReduction_32
happyReduction_32 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn35
		 (QIdent (mkPosToken happy_var_1)
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  36 happyReduction_33
happyReduction_33 (HappyTerminal (PT _ (T_CustomOperator happy_var_1)))
	 =  HappyAbsSyn36
		 (CustomOperator (happy_var_1)
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  37 happyReduction_34
happyReduction_34 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn37
		 (Quartz.Syntax.AbsQuartz.Qualified happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  37 happyReduction_35
happyReduction_35 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn37
		 (Quartz.Syntax.AbsQuartz.DefaultScope happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  38 happyReduction_36
happyReduction_36 (HappyAbsSyn48  happy_var_2)
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn38
		 (Quartz.Syntax.AbsQuartz.Prog (reverse happy_var_1) (reverse happy_var_2)
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  39 happyReduction_37
happyReduction_37  =  HappyAbsSyn39
		 ([]
	)

happyReduce_38 = happySpecReduce_3  39 happyReduction_38
happyReduction_38 _
	(HappyAbsSyn40  happy_var_2)
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn39
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  40 happyReduction_39
happyReduction_39 (HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn40
		 (Quartz.Syntax.AbsQuartz.NormalImport happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happyReduce 10 41 happyReduction_40
happyReduction_40 ((HappyAbsSyn54  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn49  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_3) `HappyStk`
	(HappyAbsSyn35  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn41
		 (Quartz.Syntax.AbsQuartz.Func happy_var_2 (reverse happy_var_3) happy_var_5 happy_var_8 happy_var_10
	) `HappyStk` happyRest

happyReduce_41 = happyReduce 8 41 happyReduction_41
happyReduction_41 ((HappyAbsSyn54  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_3) `HappyStk`
	(HappyAbsSyn35  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn41
		 (Quartz.Syntax.AbsQuartz.ParameterlessFunc happy_var_2 (reverse happy_var_3) happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_42 = happyReduce 12 41 happyReduction_42
happyReduction_42 ((HappyAbsSyn54  happy_var_12) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn45  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn45  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_3) `HappyStk`
	(HappyAbsSyn36  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn41
		 (Quartz.Syntax.AbsQuartz.Operator happy_var_2 (reverse happy_var_3) happy_var_5 happy_var_7 happy_var_10 happy_var_12
	) `HappyStk` happyRest

happyReduce_43 = happyReduce 6 41 happyReduction_43
happyReduction_43 ((HappyAbsSyn54  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn41
		 (Quartz.Syntax.AbsQuartz.Value happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_44 = happyReduce 4 41 happyReduction_44
happyReduction_44 ((HappyAbsSyn54  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn41
		 (Quartz.Syntax.AbsQuartz.ValueInferred happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_45 = happyReduce 5 41 happyReduction_45
happyReduction_45 ((HappyAbsSyn44  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn63  happy_var_3) `HappyStk`
	(HappyAbsSyn35  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn41
		 (Quartz.Syntax.AbsQuartz.Data happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_46 = happySpecReduce_2  42 happyReduction_46
happyReduction_46 (HappyAbsSyn43  happy_var_2)
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn42
		 (Quartz.Syntax.AbsQuartz.DataConstructor happy_var_1 happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_0  43 happyReduction_47
happyReduction_47  =  HappyAbsSyn43
		 ([]
	)

happyReduce_48 = happySpecReduce_1  43 happyReduction_48
happyReduction_48 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn43
		 ((:[]) happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  43 happyReduction_49
happyReduction_49 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn43
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  44 happyReduction_50
happyReduction_50 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn44
		 ((:[]) happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  44 happyReduction_51
happyReduction_51 (HappyAbsSyn44  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn44
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  45 happyReduction_52
happyReduction_52 (HappyAbsSyn50  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn45
		 (Quartz.Syntax.AbsQuartz.Argument happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happyReduce 5 45 happyReduction_53
happyReduction_53 ((HappyAbsSyn54  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn50  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn45
		 (Quartz.Syntax.AbsQuartz.ArgumentWithDefault happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_3  46 happyReduction_54
happyReduction_54 _
	(HappyAbsSyn35  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Quartz.Syntax.AbsQuartz.FreeTypeVariable happy_var_2
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_0  47 happyReduction_55
happyReduction_55  =  HappyAbsSyn47
		 ([]
	)

happyReduce_56 = happySpecReduce_2  47 happyReduction_56
happyReduction_56 (HappyAbsSyn46  happy_var_2)
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn47
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_0  48 happyReduction_57
happyReduction_57  =  HappyAbsSyn48
		 ([]
	)

happyReduce_58 = happySpecReduce_3  48 happyReduction_58
happyReduction_58 _
	(HappyAbsSyn41  happy_var_2)
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_0  49 happyReduction_59
happyReduction_59  =  HappyAbsSyn49
		 ([]
	)

happyReduce_60 = happySpecReduce_1  49 happyReduction_60
happyReduction_60 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn49
		 ((:[]) happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  49 happyReduction_61
happyReduction_61 (HappyAbsSyn49  happy_var_3)
	_
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn49
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  50 happyReduction_62
happyReduction_62 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn50
		 (Quartz.Syntax.AbsQuartz.Atom happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  50 happyReduction_63
happyReduction_63 _
	 =  HappyAbsSyn50
		 (Quartz.Syntax.AbsQuartz.UnitAtom
	)

happyReduce_64 = happySpecReduce_3  50 happyReduction_64
happyReduction_64 _
	(HappyAbsSyn50  happy_var_2)
	_
	 =  HappyAbsSyn50
		 (happy_var_2
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  51 happyReduction_65
happyReduction_65 (HappyAbsSyn50  happy_var_3)
	_
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn50
		 (Quartz.Syntax.AbsQuartz.Abstraction happy_var_1 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_2  51 happyReduction_66
happyReduction_66 (HappyAbsSyn43  happy_var_2)
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn50
		 (Quartz.Syntax.AbsQuartz.Constructor happy_var_1 happy_var_2
	)
happyReduction_66 _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  51 happyReduction_67
happyReduction_67 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  52 happyReduction_68
happyReduction_68 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn43
		 ((:[]) happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_2  52 happyReduction_69
happyReduction_69 (HappyAbsSyn43  happy_var_2)
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn43
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_69 _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  53 happyReduction_70
happyReduction_70 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn50
		 (happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  54 happyReduction_71
happyReduction_71 (HappyAbsSyn54  happy_var_3)
	(HappyAbsSyn36  happy_var_2)
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.ECustomOp happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happyReduce 4 54 happyReduction_72
happyReduction_72 ((HappyAbsSyn54  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.ELambda happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_73 = happySpecReduce_1  54 happyReduction_73
happyReduction_73 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn54
		 (happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_2  55 happyReduction_74
happyReduction_74 (HappyAbsSyn54  happy_var_2)
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.EApp happy_var_1 happy_var_2
	)
happyReduction_74 _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_2  55 happyReduction_75
happyReduction_75 (HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.ENegInt happy_var_2
	)
happyReduction_75 _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_2  55 happyReduction_76
happyReduction_76 (HappyAbsSyn34  happy_var_2)
	_
	 =  HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.ENegDouble happy_var_2
	)
happyReduction_76 _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  55 happyReduction_77
happyReduction_77 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn54
		 (happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happyReduce 6 56 happyReduction_78
happyReduction_78 ((HappyAbsSyn54  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn54  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn54  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.EIfThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_79 = happySpecReduce_1  56 happyReduction_79
happyReduction_79 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.EVar happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  56 happyReduction_80
happyReduction_80 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.EStr happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  56 happyReduction_81
happyReduction_81 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.EInt happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  56 happyReduction_82
happyReduction_82 (HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.EDouble happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  56 happyReduction_83
happyReduction_83 _
	 =  HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.EUndefined
	)

happyReduce_84 = happySpecReduce_1  56 happyReduction_84
happyReduction_84 _
	 =  HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.EUnit
	)

happyReduce_85 = happyReduce 4 56 happyReduction_85
happyReduction_85 (_ `HappyStk`
	(HappyAbsSyn54  happy_var_3) `HappyStk`
	(HappyAbsSyn48  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.EBlock (reverse happy_var_2) happy_var_3
	) `HappyStk` happyRest

happyReduce_86 = happySpecReduce_3  56 happyReduction_86
happyReduction_86 _
	(HappyAbsSyn59  happy_var_2)
	_
	 =  HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.EList happy_var_2
	)
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happyReduce 5 56 happyReduction_87
happyReduction_87 (_ `HappyStk`
	(HappyAbsSyn59  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn54  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.ETuple happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_88 = happyReduce 6 56 happyReduction_88
happyReduction_88 (_ `HappyStk`
	(HappyAbsSyn62  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn54  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.EMatch happy_var_2 (reverse happy_var_5)
	) `HappyStk` happyRest

happyReduce_89 = happyReduce 4 56 happyReduction_89
happyReduction_89 (_ `HappyStk`
	(HappyAbsSyn65  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.EDo happy_var_3
	) `HappyStk` happyRest

happyReduce_90 = happySpecReduce_3  56 happyReduction_90
happyReduction_90 _
	(HappyAbsSyn54  happy_var_2)
	_
	 =  HappyAbsSyn54
		 (happy_var_2
	)
happyReduction_90 _ _ _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  57 happyReduction_91
happyReduction_91 (HappyAbsSyn54  happy_var_3)
	_
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.EAdd happy_var_1 happy_var_3
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_3  57 happyReduction_92
happyReduction_92 (HappyAbsSyn54  happy_var_3)
	_
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.ESub happy_var_1 happy_var_3
	)
happyReduction_92 _ _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  57 happyReduction_93
happyReduction_93 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn54
		 (happy_var_1
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_3  58 happyReduction_94
happyReduction_94 (HappyAbsSyn54  happy_var_3)
	_
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.EMul happy_var_1 happy_var_3
	)
happyReduction_94 _ _ _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  58 happyReduction_95
happyReduction_95 (HappyAbsSyn54  happy_var_3)
	_
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn54
		 (Quartz.Syntax.AbsQuartz.EDiv happy_var_1 happy_var_3
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  58 happyReduction_96
happyReduction_96 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn54
		 (happy_var_1
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_0  59 happyReduction_97
happyReduction_97  =  HappyAbsSyn59
		 ([]
	)

happyReduce_98 = happySpecReduce_1  59 happyReduction_98
happyReduction_98 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn59
		 ((:[]) happy_var_1
	)
happyReduction_98 _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_3  59 happyReduction_99
happyReduction_99 (HappyAbsSyn59  happy_var_3)
	_
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn59
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_99 _ _ _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_1  60 happyReduction_100
happyReduction_100 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn54
		 (happy_var_1
	)
happyReduction_100 _  = notHappyAtAll 

happyReduce_101 = happyReduce 5 61 happyReduction_101
happyReduction_101 ((HappyAbsSyn54  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn63  happy_var_3) `HappyStk`
	(HappyAbsSyn37  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn61
		 (Quartz.Syntax.AbsQuartz.SimpleCase happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_102 = happySpecReduce_0  62 happyReduction_102
happyReduction_102  =  HappyAbsSyn62
		 ([]
	)

happyReduce_103 = happySpecReduce_2  62 happyReduction_103
happyReduction_103 (HappyAbsSyn61  happy_var_2)
	(HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn62
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_103 _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_0  63 happyReduction_104
happyReduction_104  =  HappyAbsSyn63
		 ([]
	)

happyReduce_105 = happySpecReduce_2  63 happyReduction_105
happyReduction_105 (HappyAbsSyn63  happy_var_2)
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn63
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_105 _ _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_1  64 happyReduction_106
happyReduction_106 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn64
		 (Quartz.Syntax.AbsQuartz.DoExp happy_var_1
	)
happyReduction_106 _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_3  64 happyReduction_107
happyReduction_107 (HappyAbsSyn54  happy_var_3)
	_
	(HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn64
		 (Quartz.Syntax.AbsQuartz.DoLet happy_var_1 happy_var_3
	)
happyReduction_107 _ _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_1  65 happyReduction_108
happyReduction_108 (HappyAbsSyn64  happy_var_1)
	 =  HappyAbsSyn65
		 ((:[]) happy_var_1
	)
happyReduction_108 _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_3  65 happyReduction_109
happyReduction_109 (HappyAbsSyn65  happy_var_3)
	_
	(HappyAbsSyn64  happy_var_1)
	 =  HappyAbsSyn65
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_109 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 103 103 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 66;
	PT _ (TS _ 2) -> cont 67;
	PT _ (TS _ 3) -> cont 68;
	PT _ (TS _ 4) -> cont 69;
	PT _ (TS _ 5) -> cont 70;
	PT _ (TS _ 6) -> cont 71;
	PT _ (TS _ 7) -> cont 72;
	PT _ (TS _ 8) -> cont 73;
	PT _ (TS _ 9) -> cont 74;
	PT _ (TS _ 10) -> cont 75;
	PT _ (TS _ 11) -> cont 76;
	PT _ (TS _ 12) -> cont 77;
	PT _ (TS _ 13) -> cont 78;
	PT _ (TS _ 14) -> cont 79;
	PT _ (TS _ 15) -> cont 80;
	PT _ (TS _ 16) -> cont 81;
	PT _ (TS _ 17) -> cont 82;
	PT _ (TS _ 18) -> cont 83;
	PT _ (TS _ 19) -> cont 84;
	PT _ (TS _ 20) -> cont 85;
	PT _ (TS _ 21) -> cont 86;
	PT _ (TS _ 22) -> cont 87;
	PT _ (TS _ 23) -> cont 88;
	PT _ (TS _ 24) -> cont 89;
	PT _ (TS _ 25) -> cont 90;
	PT _ (TS _ 26) -> cont 91;
	PT _ (TS _ 27) -> cont 92;
	PT _ (TS _ 28) -> cont 93;
	PT _ (TS _ 29) -> cont 94;
	PT _ (TS _ 30) -> cont 95;
	PT _ (TS _ 31) -> cont 96;
	PT _ (TS _ 32) -> cont 97;
	PT _ (TL happy_dollar_dollar) -> cont 98;
	PT _ (TI happy_dollar_dollar) -> cont 99;
	PT _ (TD happy_dollar_dollar) -> cont 100;
	PT _ (T_QIdent _) -> cont 101;
	PT _ (T_CustomOperator happy_dollar_dollar) -> cont 102;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 103 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pQualifiedIdentifier tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn37 z -> happyReturn z; _other -> notHappyAtAll })

pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn38 z -> happyReturn z; _other -> notHappyAtAll })

pListImport tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pImport tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn40 z -> happyReturn z; _other -> notHappyAtAll })

pDeclaration tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn41 z -> happyReturn z; _other -> notHappyAtAll })

pDataCase tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn42 z -> happyReturn z; _other -> notHappyAtAll })

pListType tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn43 z -> happyReturn z; _other -> notHappyAtAll })

pListDataCase tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn44 z -> happyReturn z; _other -> notHappyAtAll })

pArg tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn45 z -> happyReturn z; _other -> notHappyAtAll })

pTypeQualifier tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn46 z -> happyReturn z; _other -> notHappyAtAll })

pListTypeQualifier tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn47 z -> happyReturn z; _other -> notHappyAtAll })

pListDeclaration tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_11 tks) (\x -> case x of {HappyAbsSyn48 z -> happyReturn z; _other -> notHappyAtAll })

pListArg tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_12 tks) (\x -> case x of {HappyAbsSyn49 z -> happyReturn z; _other -> notHappyAtAll })

pType2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_13 tks) (\x -> case x of {HappyAbsSyn50 z -> happyReturn z; _other -> notHappyAtAll })

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_14 tks) (\x -> case x of {HappyAbsSyn50 z -> happyReturn z; _other -> notHappyAtAll })

pListType2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_15 tks) (\x -> case x of {HappyAbsSyn43 z -> happyReturn z; _other -> notHappyAtAll })

pType1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_16 tks) (\x -> case x of {HappyAbsSyn50 z -> happyReturn z; _other -> notHappyAtAll })

pExp1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_17 tks) (\x -> case x of {HappyAbsSyn54 z -> happyReturn z; _other -> notHappyAtAll })

pExp2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_18 tks) (\x -> case x of {HappyAbsSyn54 z -> happyReturn z; _other -> notHappyAtAll })

pExp5 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_19 tks) (\x -> case x of {HappyAbsSyn54 z -> happyReturn z; _other -> notHappyAtAll })

pExp3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_20 tks) (\x -> case x of {HappyAbsSyn54 z -> happyReturn z; _other -> notHappyAtAll })

pExp4 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_21 tks) (\x -> case x of {HappyAbsSyn54 z -> happyReturn z; _other -> notHappyAtAll })

pListExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_22 tks) (\x -> case x of {HappyAbsSyn59 z -> happyReturn z; _other -> notHappyAtAll })

pExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_23 tks) (\x -> case x of {HappyAbsSyn54 z -> happyReturn z; _other -> notHappyAtAll })

pCase tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_24 tks) (\x -> case x of {HappyAbsSyn61 z -> happyReturn z; _other -> notHappyAtAll })

pListCase tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_25 tks) (\x -> case x of {HappyAbsSyn62 z -> happyReturn z; _other -> notHappyAtAll })

pListQIdent tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_26 tks) (\x -> case x of {HappyAbsSyn63 z -> happyReturn z; _other -> notHappyAtAll })

pDoClause tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_27 tks) (\x -> case x of {HappyAbsSyn64 z -> happyReturn z; _other -> notHappyAtAll })

pListDoClause tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_28 tks) (\x -> case x of {HappyAbsSyn65 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/home/radeusgd/.stack/programs/x86_64-linux/ghc-8.2.2/lib/ghc-8.2.2/include/ghcversion.h" #-}















{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc7235_0/ghc_2.h" #-}


































































































































































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
