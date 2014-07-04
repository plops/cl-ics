# cl-ics

## description

cl-ics is a foreign function binding to call libics from common lisp
using cffi. This library allows to read and write files in the image
cytometry standard format.

## usage:

```
%get-lib-version => string
```

Returns a string like "1.5.2".

```
version filename &optional (append-extension t)
```

Returns 0 if it is not an ICS file, or the version number if it is.


```
load-preview filename &optional (plane-number 0)
```

Read a preview (2D) image out of an ICS file. Returns an uint8 array.


```
open filename mode => ics
```

Open an ICS file for reading (mode = "r") or writing (mode =
"w"). Returns an ICS handle (an opaque pointer).  When writing, append
a "2" to the mode string to create an ICS version 2.0 file. Append an
"f" to mode if, when reading, you want to force the file name to not
change (no ".ics" is appended). Append a "l" to mode if, when reading,
you don't want the locale forced to "C" (to read ICS files written
with some other locale, set the locale properly then open the file
with "rl")


```
%close ics
```

Close the ICS file. The ics 'stream' is no longer valid after this.
No files are actually written until this function is called.


```
set-layout ics datatype dimensions
```
Retrieve the layout of an ICS image. Only valid if reading. 


```C
ICSEXPORT Ics_Error IcsGetLayout (ICS const* ics, Ics_DataType* dt, int* ndims, size_t* dims);
/* Retrieve the layout of an ICS image. Only valid if reading. */

ICSEXPORT Ics_Error IcsSetLayout (ICS* ics, Ics_DataType dt, int ndims, size_t const* dims);
/* Set the layout for an ICS image. Only valid if writing. */

ICSEXPORT size_t IcsGetDataSize (ICS const* ics);
ICSEXPORT size_t IcsGetImelSize (ICS const* ics);
ICSEXPORT size_t IcsGetImageSize (ICS const* ics);
/* These three functions retrieve info from the ICS file.
 * IcsGetDataSize(ics) == IcsGetImelSize(ics) * IcsGetImageSize(ics) */

ICSEXPORT Ics_Error IcsGetData (ICS* ics, void* dest, size_t n);
/* Read the image data from an ICS file. Only valid if reading. */

ICSEXPORT Ics_Error IcsGetROIData (ICS* ics, size_t const* offset,
                                   size_t const* size, size_t const* sampling,
                                   void* dest, size_t n);
/* Read a square region of the image from an ICS file. To use the defaults
 * in one of the parameters, set the pointer to NULL. Only valid if reading. */

ICSEXPORT Ics_Error IcsGetDataWithStrides (ICS* ics, void* dest, size_t n,
                                           size_t const* stride, int ndims);
/* Read the image from an ICS file into a sub-block of a memory block. To
 * use the defaults in one of the parameters, set the pointer to NULL. Only
 * valid if reading. */

ICSEXPORT Ics_Error IcsGetDataBlock (ICS* ics, void* dest, size_t n);
/* Read a portion of the image data from an ICS file. Only valid if
 * reading. */

ICSEXPORT Ics_Error IcsSkipDataBlock (ICS* ics, size_t n);
/* Skip a portion of the image from an ICS file. Only valid if
 * reading. */

ICSEXPORT Ics_Error IcsGetPreviewData (ICS* ics, void* dest, size_t n,
                                       size_t planenumber);
/* Read a plane of the image data from an ICS file, and convert it
 * to uint8. Only valid if reading. */

ICSEXPORT Ics_Error IcsSetData (ICS* ics, void const* src, size_t n);
/* Set the image data for an ICS image. The pointer to this data must
 * be accessible until IcsClose has been called. Only valid if writing. */

ICSEXPORT Ics_Error IcsSetDataWithStrides (ICS* ics, void const* src, size_t n,
                                           size_t const* strides, int ndims);
/* Set the image data for an ICS image. The pointer to this data must
 * be accessible until IcsClose has been called. Only valid if writing. */

ICSEXPORT Ics_Error IcsSetSource (ICS* ics, char const* fname, size_t offset);
/* Set the image source parameter for an ICS version 2.0 file. Only
 * valid if writing. */

ICSEXPORT Ics_Error IcsSetCompression (ICS* ics, Ics_Compression compression, int level);
/* Set the compression method and compression parameter. Only valid if writing. */

ICSEXPORT Ics_Error IcsGetPosition (ICS const* ics, int dimension, double* origin,
                                    double* scale, char* units);
/* Get the position of the image in the real world: the origin of the first
 * pixel, the distances between pixels and the units in which to measure.
 * If you are not interested in one of the parameters, set the pointer to NULL.
 * Dimensions start at 0. Only valid if reading. */

ICSEXPORT Ics_Error IcsSetPosition (ICS* ics, int dimension, double origin,
                                    double scale, char const* units);
/* Set the position of the image in the real world: the origin of the first
 * pixel, the distances between pixels and the units in which to measure.
 * If units is NULL or empty, it is set to the default value of "undefined".
 * Dimensions start at 0. Only valid if writing. */

ICSEXPORT Ics_Error IcsGetOrder (ICS const* ics, int dimension, char* order, char* label);
/* Get the ordering of the dimensions in the image. The ordering is defined
 * by names and labels for each dimension. The defaults are x, y, z, t (time)
 * and p (probe). Dimensions start at 0. Only valid if reading. */

ICSEXPORT Ics_Error IcsSetOrder (ICS* ics, int dimension, char const* order,
                                 char const* label);
/* Set the ordering of the dimensions in the image. The ordering is defined
 * by providing names and labels for each dimension. The defaults are
 * x, y, z, t (time) and p (probe). Dimensions start at 0. Only valid if writing. */

ICSEXPORT Ics_Error IcsGetCoordinateSystem (ICS const* ics, char* coord);
/* Get the coordinate system used in the positioning of the pixels.
 * Related to IcsGetPosition(). The default is "video". Only valid if
 * reading. */

ICSEXPORT Ics_Error IcsSetCoordinateSystem (ICS* ics, char const* coord);
/* Set the coordinate system used in the positioning of the pixels.
 * Related to IcsSetPosition(). The default is "video". Only valid if
 * writing. */

ICSEXPORT Ics_Error IcsGetSignificantBits (ICS const* ics, size_t* nbits);
/* Get the number of significant bits. Only valid if reading. */

ICSEXPORT Ics_Error IcsSetSignificantBits (ICS* ics, size_t nbits);
/* Set the number of significant bits. Only valid if writing. */

ICSEXPORT Ics_Error IcsGetImelUnits (ICS const* ics, double* origin, double* scale, char* units);
/* Set the position of the pixel values: the offset and scaling, and the
 * units in which to measure. If you are not interested in one of the
 * parameters, set the pointer to NULL. Only valid if reading. */

ICSEXPORT Ics_Error IcsSetImelUnits (ICS* ics, double origin, double scale, char const* units);
/* Set the position of the pixel values: the offset and scaling, and the
 * units in which to measure. If units is NULL or empty, it is set to the
 * default value of "relative". Only valid if writing. */

ICSEXPORT Ics_Error IcsGetScilType (ICS const* ics, char* sciltype);
/* Get the string for the SCIL_TYPE parameter. This string is used only
 * by SCIL_Image. Only valid if reading. */

ICSEXPORT Ics_Error IcsSetScilType (ICS* ics, char const* sciltype);
/* Set the string for the SCIL_TYPE parameter. This string is used only
 * by SCIL_Image. It is required if you want to read the image using
 * SCIL_Image. Only valid if writing. */

ICSEXPORT Ics_Error IcsGuessScilType (ICS* ics);
/* As IcsSetScilType, but creates a string according to the DataType
 * in the ICS structure. It can create a string for g2d, g3d, f2d, f3d,
 * c2d and c3d. Only valid if writing. */

ICSEXPORT char const* IcsGetErrorText (Ics_Error error);
/* Returns a textual representation of an error. */

ICSEXPORT Ics_Error IcsAddHistoryString (ICS* ics, char const* key, char const* value);
/* Add history lines to the ICS file. key can be NULL */
#define IcsAddHistory IcsAddHistoryString

ICSEXPORT Ics_Error IcsDeleteHistory (ICS* ics, char const* key);
/* Delete all history lines with key from ICS file. key can be NULL,
 * deletes all. */

ICSEXPORT Ics_Error IcsGetNumHistoryStrings (ICS* ics, int* num);
/* Get the number of HISTORY lines from the ICS file. */

ICSEXPORT Ics_Error IcsGetHistoryString (ICS* ics, char* string,
                                         Ics_HistoryWhich which);
/* Get history line from the ICS file. string must have at least
 * ICS_LINE_LENGTH characters allocated. */

ICSEXPORT Ics_Error IcsGetHistoryKeyValue (ICS* ics, char* key, char* value,
                                           Ics_HistoryWhich which);
/* Get history line from the ICS file as key/value pair. key must have
 * ICS_STRLEN_TOKEN characters allocated, and value ICS_LINE_LENGTH.
 * key can be null, token will be discarded. */

ICSEXPORT Ics_Error IcsNewHistoryIterator (ICS* ics, Ics_HistoryIterator* it, char const* key);
/* Initializes history iterator. key can be NULL. */

ICSEXPORT Ics_Error IcsGetHistoryStringI (ICS* ics, Ics_HistoryIterator* it, char* string);
/* Get history line from the ICS file using iterator. string must have at
 * least ICS_LINE_LENGTH characters allocated. */

ICSEXPORT Ics_Error IcsGetHistoryKeyValueI (ICS* ics, Ics_HistoryIterator* it,
                                            char* key, char* value);
/* Get history line from the ICS file as key/value pair using iterator.
 * key must have ICS_STRLEN_TOKEN characters allocated, and value
 * ICS_LINE_LENGTH. key can be null, token will be discarded. */

ICSEXPORT Ics_Error IcsDeleteHistoryStringI (ICS* ics, Ics_HistoryIterator* it);
/* Delete last retrieved history line (iterator still points to the same string). */

ICSEXPORT Ics_Error IcsReplaceHistoryStringI (ICS* ics, Ics_HistoryIterator* it,
                                              char const* key, char const* value);
/* Delete last retrieved history line (iterator still points to the same string). */
```

## example:


```common-lisp
```
