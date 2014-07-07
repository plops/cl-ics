(in-package :ics)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load-ics-libraries))

;; function definitions

(defcfun (%get-lib-version "IcsGetLibVersion") :string)

(defcfun (%version "IcsVersion") :int
  (filename :string)
  (forcename :int))

(defcenum ics-error
  (:err-Ok 0)
  :Err-FSizeConflict
  :Err-OutputNotFilled
  :Err-Alloc
  :Err-BitsVsSizeConfl
  :Err-BlockNotAllowed
  :Err-BufferTooSmall
  :Err-CompressionProblem
  :Err-CorruptedStream
  :Err-DecompressionProblem
  :Err-DuplicateData
  :Err-EmptyField
  :Err-EndOfHistory
  :Err-EndOfStream
  :Err-FCloseIcs
  :Err-FCloseIds
  :Err-FCopyIds
  :Err-FOpenIcs
  :Err-FOpenIds
  :Err-FReadIcs
  :Err-FReadIds
  :Err-FTempMoveIcs
  :Err-FWriteIcs
  :Err-FWriteIds
  :Err-FailWriteLine
  :Err-IllIcsToken
  :Err-IllParameter
  :Err-IllegalROI
  :Err-LineOverflow
  :Err-MissBits
  :Err-MissCat
  :Err-MissLayoutSubCat
  :Err-MissParamSubCat
  :Err-MissRepresSubCat
  :Err-MissSensorSubCat
  :Err-MissSensorSubSubCat
  :Err-MissSubCat
  :Err-MissingData
  :Err-NoLayout
  :Err-NoScilType
  :Err-NotIcsFile
  :Err-NotValidAction
  :Err-TooManyChans
  :Err-TooManyDims
  :Err-UnknownCompression
  :Err-UnknownDataType
  :Err-WrongZlibVersion )





(defcfun (%get-error-text "IcsGetErrorText") :string
  (err ics-error))

(defctype sizet :unsigned-int) ;; FIXME this might be necessary to be
				;; grovelled

(defcfun (%load-preview "IcsLoadPreview") ics-error
  (filename :string)
  (plane-number sizet)
  (dst :pointer)
  (w (:pointer sizet))
  (h (:pointer sizet)))


(defcfun (%free "free") :void (ptr :pointer))

(defcfun (%open "IcsOpen") ics-error
  (ics :pointer)
  (filename :string)
  (mode :string))

(defcfun (%close "IcsClose") ics-error
  (ics :pointer))

(defcenum ics-datatype
  (:ics-unknown 0)
  :uint8
  :sint8
  :uint16
  :sint16
  :uint32
  :sint32
  :real32
  :real64
  :complex32
  :complex64)

;; (gethash :complex32 (cffi::keyword-values (funcall (cffi::find-type-parser 'ics-datatype))))


(defcfun (%set-layout "IcsSetLayout") ics-error
  (ics :pointer)
  (datatype ics-datatype)
  (ndims :int)
  (dims :pointer))

(defcfun (%set-data "IcsSetData") ics-error
  (ics :pointer)
  (src :pointer)
  (n sizet))

(defcenum ics-compression
  (:compression-uncompressed 0)
  :compression-compress
  :compression-gzip)

(defcfun (%set-compression "IcsSetCompression") ics-error
  (ics :pointer)
  (compression ics-compression)
  (level :int))

(defcfun (%get-data-size "IcsGetDataSize") sizet
  (ics :pointer))
