// okra.h
//
// author: Erik Winkels (aerique@xs4all.nl)
//
// See the LICENSE file in the Okra root directory for more info.

#include "Ogre.h"

using namespace Ogre;


#if OGRE_DOUBLE_PRECISION == 1
    typedef double okraReal;
#else
    typedef float okraReal;
#endif

//typedef okraReal okraMatrix4[16];
//typedef okraReal okraQuaternion[4];
//typedef okraReal okraVector3[3];

typedef okraReal okraArray2[2];
typedef okraReal okraArray3[3];
typedef okraReal okraArray4[4];
typedef okraReal okraArray6[6];
typedef okraReal okraArray9[9];
typedef okraReal okraArray16[16];


typedef enum okra_types {
    OKRA_INT,
    OKRA_POINTER,
    OKRA_REAL,
    OKRA_STRING
} okra_type;


// For later: try out if it works to put all the *_arrays in a union.
typedef struct {
    size_t length;
    okra_type type;
    int* int_array;
    size_t* pointer_array;
    okraReal* real_array;
    const char** string_array;
} array_shuttle ;
