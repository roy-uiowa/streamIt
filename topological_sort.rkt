#lang racket
(require graph)
(define g (unweighted-graph/directed null))
(add-vertex! g 'rp_FMRadio)

;(add-vertex! g 'f_FloatOneSource)
(add-directed-edge! g 'rp_FMRadio 'f_FloatOneSource)

;(add-vertex! g 'p_FMRadioCore)
(add-directed-edge! g 'f_FloatOneSource 'p_FMRadioCore)

;(add-vertex! g 'f_LowPassFilter)
(add-directed-edge! g 'p_FMRadioCore 'f_LowPassFilter)

;(add-vertex! g 'f_FMDemodulator)
(add-directed-edge! g 'f_LowPassFilter 'f_FMDemodulator)

;(add-vertex! g 'p_Equalizer)
(add-directed-edge! g 'f_FMDemodulator 'p_Equalizer)

;(add-vertex! g 'sj_Eqsplit)
(add-directed-edge! g 'p_Equalizer 'sj_Eqsplit)

;;;;;;;;;;;;;;;;;;;;//\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(add-vertex! g 'p_temp)
(add-directed-edge! g 'sj_Eqsplit 'p_temp)
;(add-directed-edge! g 'p_temp 'sj_Eqsplit)

;(add-vertex! g 'p_BandPassFilter)
(add-directed-edge! g 'p_temp 'p_BandPassFilter)

;(add-vertex! g 'sj_BPFCore)
(add-directed-edge! g 'p_BandPassFilter 'sj_BPFCore)
;;/\
;(add-vertex! g 'f_LowPassFilter_1)
(add-edge! g 'sj_BPFCore 'f_LowPassFilter_1)

;(add-vertex! g 'f_LowPassFilter_2)
(add-edge! g 'sj_BPFCore 'f_LowPassFilter_2)
;;\/

;(add-vertex! g 'p_subtracter)
(add-directed-edge! g 'sj_BPFCore 'p_subtracter)

;(add-vertex! g 'f_Amplify)
(add-directed-edge! g 'p_subtracter 'f_Amplify)
(add-directed-edge! g 'f_Amplify 'sj_Eqsplit)
;;;;;;;;;;;;;;;;;;;;;;;/\\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(add-vertex! g 'p_temp_1)
(add-directed-edge! g 'sj_Eqsplit 'p_temp_1)

;(add-vertex! g 'p_BandPassFilter_1)
(add-directed-edge! g 'p_temp_1 'p_BandPassFilter_1)

(add-vertex! g 'sj_BPFCore_1)
(add-directed-edge! g 'p_BandPassFilter_1 'sj_BPFCore_1)
;;/\
(add-vertex! g 'f_LowPassFilter_3)
(add-edge! g 'sj_BPFCore_1 'f_LowPassFilter_3)

(add-vertex! g 'f_LowPassFilter_4)
(add-edge! g 'sj_BPFCore_1 'f_LowPassFilter_4)
;;\/

(add-vertex! g 'p_subtracter_1)
(add-directed-edge! g 'sj_BPFCore 'p_subtracter_1)

(add-vertex! g 'f_Amplify_1)
(add-directed-edge! g 'p_subtracter_1 'f_Amplify_1)
(add-directed-edge! g 'f_Amplify_1 'sj_Eqsplit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-vertex! g 'f_tempFilter)
(add-directed-edge! g 'sj_Eqsplit 'f_tempFilter)

(add-vertex! g 'f_FloatOnePrinter)
(add-directed-edge! g 'f_tempFilter 'f_FloatOnePrinter)


;(get-vertices g)
;(get-edges g)

(tsort g)