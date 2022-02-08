/***************************************************************************
 * Copyright                                                               *
 *                                                                         *
 *     ESCRYPT GmbH - Embedded Security       ESCRYPT Inc.                 *
 *     Zentrum fuer IT-Sicherheit             315 E Eisenhower Parkway     *
 *     Lise-Meitner-Allee 4                   Suite 214                    *
 *     44801 Bochum                           Ann Arbor, MI 48108          *
 *     Germany                                USA                          *
 *                                                                         *
 *     http://www.escrypt.com                                              *
 *     info"at"escrypt.com                                                 *
 *                                                                         *
 * All Rights reserved                                                     *
 ***************************************************************************/

/***************************************************************************/
/*!
   \file        ecc.c

   \brief       Elliptic Curve Diffie/Hellman (ECDH) and
   Elliptic Curve Digital Signature Algorithm (ECDSA)
   GF(p) point arithmetics
   GF(p) multiprecision integer arithmetic implementations

   $Rev: 19842 $
 */
/***************************************************************************/
/***************************************************************************
 * 1. INCLUDES                                                             *
 ***************************************************************************/

#include "ecc.h"

/***************************************************************************
 * 2. DEFINES                                                              *
 ***************************************************************************/

#ifdef Esc_HAS_INT64
/* low & high 32 bit word from 64 bit */
#    define EscEcc_LO64( a ) ( (UINT32)( ( a ) & 0xFFFFFFFFU ) )
#    define EscEcc_HI64( a ) ( (UINT32)( ( ( a ) >> 32 ) & 0xFFFFFFFFU ) )
/* return min(x,y) */
#    define EscEcc_MIN( x, y ) ( ( ( x ) < ( y ) ) ? ( x ) : ( y ) )
#else
/* low & high 16 bit word from 32 bit */
#    define EscEcc_LO32( a ) ( (UINT16)( ( a ) & 0xFFFFU ) )
#    define EscEcc_HI32( a ) ( (UINT16)( ( ( a ) >> 16 ) & 0xFFFFU ) )
#endif


/** The key size in words */
#if ( EscEcc_KEY_LENGTH  == 521U )
#   define EscEcc_KEY_WORDS 17U
#else
#   define EscEcc_KEY_WORDS ( EscEcc_KEY_LENGTH / EscEcc_WORD_BITS )
#endif


/***************************************************************************
 * 3. DEFINITIONS                                                          *
 ***************************************************************************/


/*
   The functions are classified in four types:
   Word arithmetic - EscEccWd_*
   Field arithmetic - EscEccFe_*
   Point arithmetic - EscEccPt_*
   Miscellaneous - EscEcc_*
 */

/* Word arithmetic */
#ifndef Esc_HAS_INT64
static void
EscEccWd_Inc(
    UINT32* hi,
    UINT32* lo,
    UINT32 increment );

#endif
static void
EscEccWd_AddC(
    UINT32* carry,
    UINT32* c,
    const UINT32 a,
    const UINT32 b );

static void
EscEccWd_AddCLoop(
    UINT32* carry_out,
    UINT32 c[],
    const UINT32 a[],
    const UINT32 b[] );

static void
EscEccWd_SubB(
    UINT32* borrow,
    UINT32* c,
    UINT32 a,
    UINT32 b );

static void
EscEccWd_SubBLoop(
    UINT32 c[],
    UINT32* borrow_out,
    const UINT32 a[],
    const UINT32 b[] );

#ifndef Esc_HAS_INT64
static void
EscEccWd_Multiply(
    UINT32* c_high,
    UINT32* c_low,
    UINT32 a,
    UINT32 b );

#endif
static UINT32
EscEccWd_FromOctets(
    const UINT8 octets[],
    UINT32 idx );

/* Field arithmetic */

static BOOL
EscEccFe_IsOne(
    const EscEcc_FieldElementT* a );

static void
EscEccFe_ShiftRight(
    EscEcc_FieldElementT* a );

static void
EscEccFe_MultiplyShort(
    EscEcc_FieldElementLongT* c,
    const EscEcc_FieldElementT* a,
    const EscEcc_FieldElementT* b );

static void
EscEccFe_Multiply(
    EscEcc_FieldElementLongT* c,
    const EscEcc_FieldElementT* a,
    const EscEcc_FieldElementT* b );

static void
EscEccFe_ReduceBarrett(
    EscEcc_FieldElementT* r,
    const EscEcc_FieldElementLongT* z,
    const EscEcc_FieldT* fGP );

#if ( EscEcc_CURVE_TYPE == EscEcc_secPr1 )
#   if ( EscEcc_KEY_LENGTH == 160U )
static void
EscEccFe_ReduceNIST_160(
    EscEcc_FieldElementT* r,
    const EscEcc_FieldElementLongT* z,
    const EscEcc_FieldT* fGP );

#   endif
#   if ( EscEcc_KEY_LENGTH == 192U )
static void
EscEccFe_ReduceNIST_192(
    EscEcc_FieldElementT* r,
    const EscEcc_FieldElementLongT* z,
    const EscEcc_FieldT* fGP );

#   endif
#   if ( EscEcc_KEY_LENGTH == 224U )
static void
EscEccFe_ReduceNIST_224(
    EscEcc_FieldElementT* r,
    const EscEcc_FieldElementLongT* z,
    const EscEcc_FieldT* fGP );

#   endif
#   if ( EscEcc_KEY_LENGTH == 256U )
static void
EscEccFe_ReduceNIST_256(
    EscEcc_FieldElementT* r,
    const EscEcc_FieldElementLongT* z,
    const EscEcc_FieldT* fGP );

#   endif
#   if ( EscEcc_KEY_LENGTH == 384U )
static void
EscEccFe_ReduceNIST_384(
    EscEcc_FieldElementT* r,
    const EscEcc_FieldElementLongT* z,
    const EscEcc_FieldT* fGP );

#   endif
#   if ( EscEcc_KEY_LENGTH == 521U )
static void
EscEccFe_ReduceNIST_521(
    EscEcc_FieldElementT* r,
    const EscEcc_FieldElementLongT* z,
    const EscEcc_FieldT* fGP );

#   endif /* EscEcc_KEY_LENGTH  */
#endif /* EscEcc_CURVE_TYPE */

static BOOL
EscEccFe_isBitSet(
    const EscEcc_FieldElementT* c,
    SINT16 index );

/* Point arithmetic */
static BOOL
EscEccPt_IsZero(
    const EscEcc_PointT* pX );

static void
EscEccPt_JacobianDouble(
    EscEcc_PointT* pJ,
    const EscEcc_PointT* pJ0 );


/* misc */

static BOOL
EscEcc_PartialPubKeyValidation(
    EscEcc_PointT* pQ_point,
    const EscEcc_PublicKeyT* pQ );

static void
EscEcc_MemsetZero(
    UINT32 dest[],
    UINT16 length );

/***************************************************************************
 * 4. IMPLEMENTATION OF FUNCTIONS                                          *
 ***************************************************************************/

/*****************************************************************
 Increments a 64 bit value whose low byte is in *lo and high byte
 is in *hi by the value increment.
 *****************************************************************/
#ifndef Esc_HAS_INT64
static void
EscEccWd_Inc(
    UINT32* hi,
    UINT32* lo,
    UINT32 increment )
{
    /* (a+b)>0xffffffff => a > 0xffffffff - b */
    if ( *lo > ( 0xffffffffU - increment ) ) {
        ( *hi )++;
    }
    *lo += increment;
}

#endif
/*****************************************************************
 Adds two FWORDs to another FWORD + carry
 c = a + b + carry
 *****************************************************************/
static void
EscEccWd_AddC(
    UINT32* carry,
    UINT32* c,
    const UINT32 a,
    const UINT32 b )
{
#ifdef Esc_HAS_INT64
    UINT64 u64_temp = (UINT64)a + (UINT64)b + (UINT64)*carry;
    *carry = EscEcc_HI64( u64_temp );
    *c = EscEcc_LO64( u64_temp );
#else
    *c = *carry;
    *carry = 0U;
    EscEccWd_Inc( carry, c, a );
    EscEccWd_Inc( carry, c, b );
#endif
}

/*****************************************************************
Adds two arrays of FWORDs with a length of EscEcc_MAX_WORDS,
 with carry
*****************************************************************/
static void
EscEccWd_AddCLoop(
    UINT32* carry_out,
    UINT32 c[],
    const UINT32 a[],
    const UINT32 b[] )
{
    UINT16 i;
    *carry_out = 0U;
    for ( i = 0U; i < EscEcc_MAX_WORDS; i++ ) {
        EscEccWd_AddC( carry_out, &c[ i ], a[ i ], b[ i ] );
    }
}

/*****************************************************************
Calculates c = a - b + borrow
****************************************************************/
static void
EscEccWd_SubB(
    UINT32* borrow,
    UINT32* c,
    UINT32 a,
    UINT32 b )
{
#ifdef Esc_HAS_INT64
    UINT32 u32_temp_sub = ~b;
    UINT64 u64_temp = (UINT64)a + (UINT64)u32_temp_sub + (UINT64)*borrow;
    *borrow = EscEcc_HI64( u64_temp );
    *c = EscEcc_LO64( u64_temp );
#else
    UINT32 inv_b = ~b;
    EscEccWd_AddC( borrow, c, a, inv_b );
#endif
}

/*****************************************************************
Subtracts two arrays of FWORDs with a length of EscEcc_MAX_WORDS,
 with borrow
*****************************************************************/
static void
EscEccWd_SubBLoop(
    UINT32 c[],
    UINT32* borrow_out,
    const UINT32 a[],
    const UINT32 b[] )
{
    UINT16 i;
    *borrow_out = 1U;
    for ( i = 0U; i < EscEcc_MAX_WORDS; i++ ) {
        EscEccWd_SubB( borrow_out, &c[ i ], a[ i ], b[ i ] );
    }
}

/*****************************************************************
Calculates c = a * b
****************************************************************/

#ifndef Esc_HAS_INT64
static void
EscEccWd_Multiply(
    UINT32* c_high,
    UINT32* c_low,
    UINT32 a,
    UINT32 b )
{
    UINT16 a1, a0;
    UINT16 b1, b0;
    UINT32 uv;
    UINT16 u;
    UINT16 c2, c1, c0;

    /*
       We use an inlined version of the classical
       multiplication algorithm:
     */
    a0 = EscEcc_LO32( a );
    a1 = EscEcc_HI32( a );
    b0 = EscEcc_LO32( b );
    b1 = EscEcc_HI32( b );

    /* i=0, j=0 */
    uv = (UINT32)a0 * (
            UINT32)b0;
    u = EscEcc_HI32( uv );
    c0 = EscEcc_LO32( uv );

    /* j=1 */
    uv = ( (UINT32)a0 * (UINT32)b1 ) + (UINT32)u;
    c1 = EscEcc_LO32( uv );
    c2 = EscEcc_HI32( uv );

    /* i=1, j=0 */
    uv = (UINT32)c1 + ( (UINT32)a1 * (UINT32)b0 );
    u = EscEcc_HI32( uv );

    /* j=1 */
    *c_low = (UINT32)c0 + ( uv << 16 );
    *c_high = (UINT32)c2 + ( (UINT32)a1 * (UINT32)b1 ) + (UINT32)u;
}

#endif

/******************************************************
Converts an octet string in little endian format into a UINT32 value
 ******************************************************/
static UINT32
EscEccWd_FromOctets(
    const UINT8 octets[],
    UINT32 idx )
{
    UINT32 v;
    v = ( ( (UINT32)octets[ idx + 3U ] << 24 ) |
          ( (UINT32)octets[ idx + 2U ] << 16 ) |
          ( (UINT32)octets[ idx + 1U ] << 8 ) |
          ( (UINT32)octets[ idx ] )
           );
    return v;
}

/******************************************
 * substitute for memset(dest, 0, length) *
 ******************************************/
static void
EscEcc_MemsetZero(
    UINT32 dest[],
    UINT16 length )
{
    /* declarations */
    UINT16 i;
    Esc_ASSERT( length < 0xffffU );

    for ( i = 0U; i < length; i++ ) {
        dest[ i ] = 0U;
    }
}

/*****************************************************
 * Convert EscEcc_FieldElementT to UINT8                    *
 * Note: only works for field elements without sign  *
 *****************************************************/
void
EscEccFe_ToUint8(
    const EscEcc_FieldElementT* input,  /* field element to convert */
    UINT8 output[] )                    /* result */
{
    /* declarations */
    UINT16 i;
    UINT32 input_w32;

#if ( EscEcc_KEY_LENGTH == 521U)
    for ( i = 0U; i < ( EscEcc_KEY_WORDS - 1U ); i++ ) {
        input_w32 = input->word[ i ];
        output[ ( 4U * i ) ] = (UINT8)( ( input_w32 ) );
        output[ ( 4U * i ) + 1U ] = (UINT8)( ( input_w32 ) >> 8 );
        output[ ( 4U * i ) + 2U ] = (UINT8)( ( input_w32 ) >> 16 );
        output[ ( 4U * i ) + 3U ] = (UINT8)( ( input_w32 ) >> 24 );
    }
    /* copy the last two bytes 64 and 65 from the Field Element */
    input_w32 = input->word[ i ];
    output[ ( 4U * i ) ] = (UINT8)( ( input_w32 ) );
    output[ ( 4U * i ) + 1U ] = (UINT8)( ( input_w32 ) >> 8 );

#else
    for ( i = 0U; i < EscEcc_KEY_WORDS; i++ ) {
        input_w32 = input->word[ i ];
        output[ ( 4U * i ) ] = (UINT8)( ( input_w32 ) );
        output[ ( 4U * i ) + 1U ] = (UINT8)( ( input_w32 ) >> 8 );
        output[ ( 4U * i ) + 2U ] = (UINT8)( ( input_w32 ) >> 16 );
        output[ ( 4U * i ) + 3U ] = (UINT8)( ( input_w32 ) >> 24 );
    }
#endif
}

/****************************************************
 * Convert UINT8 to EscEcc_FieldElementT                   *
 * Note: only works for field elements without sign *
 ****************************************************/
void
EscEccFe_FromUint8(
    const UINT8 input[],        /* UINT8 array to convert           */
    EscEcc_FieldElementT* output )
{
    /* number of words */
    /* declarations */
    UINT16 i;

#if ( EscEcc_KEY_LENGTH == 521U)
    UINT8 inIdx;
    for ( i = 0U; i < ( EscEcc_KEY_WORDS - 1U ); i++ ) {
        inIdx = (UINT8)( i * 4U );
        output->word[ i ] = EscEccWd_FromOctets( input, (UINT32)inIdx );
    }
    /* copy the last two bytes 64 and 65 into the Field Element */
    inIdx = (UINT8)( i * 4U );
    output->word[ i ] = ( ( (UINT32)input[ inIdx + 1U ] << 8U ) |
                          ( (UINT32)input[ inIdx ] )
                        );
    for ( i = EscEcc_KEY_WORDS; i < EscEcc_MAX_WORDS; i++ ) {
        output->word[ i ] = 0U;
    }
#else
    for ( i = 0U; i < EscEcc_KEY_WORDS; i++ ) {
        UINT8 inIdx;
        inIdx = (UINT8)( i * 4U );
        output->word[ i ] = EscEccWd_FromOctets( input, (UINT32)inIdx );
    }
    for ( i = EscEcc_KEY_WORDS; i < EscEcc_MAX_WORDS; i++ ) {
        output->word[ i ] = 0U;
    }
#endif

}

#ifndef EscEcc_ECDSA_DISABLE
/****************************************************
 * Convert EscEcc_FieldElementT to                  *
 * EscEcc_FieldElementLongT                         *
 * a := b                                           *
 * Note: only works for field elements without sign *
 ****************************************************/
void
EscEccFe_ToLongElement(
    EscEcc_FieldElementLongT* a,
    const EscEcc_FieldElementT* b )
{
    UINT8 i;

    for ( i = 0U; i < EscEcc_MAX_WORDS; i++ ) {
        a->word[ i ] = b->word[ i ];
    }
    for ( i = EscEcc_MAX_WORDS; i < EscEcc_MAX_LONG_WORDS; i++ ) {
        a->word[ i ] = 0U;
    }
}

#endif

/*****************************
 * set field element to zero *
 *****************************/
void
EscEccFe_SetZero(
    EscEcc_FieldElementT* a )
{
    EscEcc_MemsetZero( a->word, EscEcc_MAX_WORDS );
}

/****************************
 * set field element to one *
 ****************************/
void
EscEccFe_SetOne(
    EscEcc_FieldElementT* a )
{
    a->word[ 0 ] = 1U;

    EscEcc_MemsetZero( &a->word[ 1 ], ( EscEcc_MAX_WORDS - 1U ) );
}

/**********************************
 * check if field element is zero *
 * note: sign irrelevant          *
 **********************************/
BOOL
EscEccFe_IsZero(
    const EscEcc_FieldElementT* a )
{
    UINT8 i;
    BOOL isZero = TRUE;

    /* for i from 0 to t do a[i] =? 0 */
    for ( i = 0U; i < EscEcc_MAX_WORDS; i++ ) {
        if ( a->word[ i ] != 0U ) {
            /* not zero */
            isZero = FALSE;
            break;
        }
    }

    return isZero;
}

/**********************************
 * checks if field element is one *
 **********************************/
static BOOL
EscEccFe_IsOne(
    const EscEcc_FieldElementT* a )
{
    /* declarations */
    BOOL isOne = TRUE;

    /* a[0] =? 1 */
    if ( a->word[ 0 ] != 1U ) {
        isOne = FALSE;
    } else {
        UINT8 i;
        /* for i from 1 to t-1 do a[i] =? 0 */
        for ( i = 1U; i < EscEcc_MAX_WORDS; i++ ) {
            if ( a->word[ i ] != 0U ) {
                /* not one */
                isOne = FALSE;
            }
        }
    }

    return isOne;
}

/*******************************************
 * compares two field elements `a' and `b' *
  +1: a>b
  -1: a<b
   0: a==b
 *******************************************/
SINT8
EscEccFe_AbsoluteCompare(
    const EscEcc_FieldElementT* a,
    const EscEcc_FieldElementT* b )
{
    SINT8 i;
    SINT8 compResult = 0;

    /* compare the corresponding words until a different word pair is found */
    for ( i = (SINT8)( EscEcc_MAX_WORDS - 1U ); ( compResult == 0 ) && ( i >= 0 ); i-- ) {
        /* next lower pair */
        if ( a->word[ i ] > b->word[ i ] ) {
            /* a > b */
            compResult = 1;
        } else if ( a->word[ i ] < b->word[ i ] ) {
            /* a < b */
            compResult = -1;
        } else {
            /* do nothing */
        }
    }

    return compResult;
}

/****************************************************
 * assigns a field element to another field element *
 ****************************************************/
void
EscEccFe_Assign(
    EscEcc_FieldElementT* c,
    const EscEcc_FieldElementT* a )
{
    UINT8 i;

    for ( i = 0U; i < EscEcc_MAX_WORDS; i++ ) {
        c->word[ i ] = a->word[ i ];
    }
}

/****************************************
 * shifts field element right by n bits *
 ****************************************/
static void
EscEccFe_ShiftRight(
    EscEcc_FieldElementT* a )
{
    UINT8 i;

    /* shift first words */
    for ( i = 0U; i < ( EscEcc_MAX_WORDS - 1U ); i++ ) {
        UINT32 v;
        v = a->word[ i ] >> 1;
        v ^= a->word[ i + 1U ] << 31;

        a->word[ i ] = v;
    }

    /* shift last word */
    a->word[ EscEcc_MAX_WORDS - 1U ] >>= 1;
}

/***********************************************************************
 * multiplies two field elements c = a * b
 Highest word will only be used if one operand is the prime number -->
 adjusted loop borders for optimization
 Note: To calculate with all words use EscEccFe_Multiply !!!
 ***********************************************************************/
static void
EscEccFe_MultiplyShort(
    EscEcc_FieldElementLongT* c,
    const EscEcc_FieldElementT* a,
    const EscEcc_FieldElementT* b )
{
    /* declarations */
    UINT8 i;
    /* specifies the number of used words */
    UINT8 MUL_MAX_WORDS;

    /* if UINT64 type is supported, use row-wise multiplication (a lot faster) */
#ifdef Esc_HAS_INT64

    UINT8 j;
    UINT8 min;
    UINT32 carry, t;
    UINT8 b_0, a_0;
    UINT64 uv_t;
    UINT64 uv;
    uv_t = 0U;
    t = 0U;

    /* for keylength 160 bit the highest 2 words are not used in almost every calculation except
       if one operand is the variable "base_point_order_n" */
#    if ( EscEcc_KEY_LENGTH == 160U )
    if ( ( a->word[ EscEcc_MAX_WORDS - 2U ] == 0U ) && ( b->word[ EscEcc_MAX_WORDS - 2U ] == 0U ) ) {
        MUL_MAX_WORDS = ( EscEcc_MAX_WORDS - 2U );
    } else {
        MUL_MAX_WORDS = ( EscEcc_MAX_WORDS - 1U );
    }
#    else
    MUL_MAX_WORDS = ( EscEcc_MAX_WORDS - 1U );
#    endif

    /* check if highest words are really not used */
    Esc_ASSERT( ( a->word[ MUL_MAX_WORDS ] == 0U ) && ( b->word[ MUL_MAX_WORDS ] == 0U ) );

    /* clear upper 2 words because they wont be calculated */
    c->word[ EscEcc_MAX_LONG_WORDS - 1U ] = 0U;
    c->word[ EscEcc_MAX_LONG_WORDS - 2U ] = 0U;
    /* for keylength 160bit the highest 4 words has to be cleared */
#    if ( EscEcc_KEY_LENGTH == 160U )
    c->word[ EscEcc_MAX_LONG_WORDS - 3U ] = 0U;
    c->word[ EscEcc_MAX_LONG_WORDS - 4U ] = 0U;
#    endif

    for ( i = 0U; i < ( 2U * ( MUL_MAX_WORDS ) ); i++ ) {
        b_0 = EscEcc_MIN( ( MUL_MAX_WORDS - 1U ), i );
        a_0 = i - b_0;
        min = EscEcc_MIN( (UINT8)( MUL_MAX_WORDS - a_0 ), ( b_0 + 1U ) );
        for ( j = 0U; j < min; j++ ) {
            /* a[a_o +j] * b[b_o - j] */
            uv = (UINT64)a->word[ a_0 + j ] * (UINT64)b->word[ b_0 - j ];
            /* sum on uv_t */
            uv_t += uv;
            /* if overflow save carry to t */
            if ( uv_t < uv ) {
                t++;
            }
        }

        /* c[i] = v, v = u, u = t, t = 0 */
        c->word[ i ] = EscEcc_LO64( uv_t );
        carry = EscEcc_HI64( uv_t );
        uv_t = carry;
        uv_t |= (UINT64)( t ) << 32U;
        t = 0U;
    }

    /* if UINT64 type is not supported, use classical multiplication */
#else

    /* for keylength 160 bit the highest 2 words are not used in almost every calculation except
       if one operand is the variable "base_point_order_n" */
#    if ( EscEcc_KEY_LENGTH == 160U )
    if ( ( a->word[ EscEcc_MAX_WORDS - 2U ] == 0U ) && ( b->word[ EscEcc_MAX_WORDS - 2U ] == 0U ) ) {
        MUL_MAX_WORDS = ( EscEcc_MAX_WORDS - 2U );
    } else {
        MUL_MAX_WORDS = ( EscEcc_MAX_WORDS - 1U );
    }
#    else
    MUL_MAX_WORDS = ( EscEcc_MAX_WORDS - 1U );
#    endif

    /* check if highest words are really not used */
    Esc_ASSERT( ( a->word[ MUL_MAX_WORDS ] == 0U ) && ( b->word[ MUL_MAX_WORDS ] == 0U ) );

    /* 1.) c = 0 */
    for ( i = 0U; i < EscEcc_MAX_LONG_WORDS; i++ ) {
        c->word[ i ] = 0U;
    }
    /* 2.) for i = 0 to EscEcc_MAX_WORDS */
    for ( i = 0U; i < ( MUL_MAX_WORDS ); i++ ) {
        UINT8 j;
        UINT32 u;
        /* 2.1) u = 0 */
        u = 0U;

        /* 2.2) for j = 0 to (MUL_MAX_WORDS) do */
        for ( j = 0U; j < ( MUL_MAX_WORDS ); j++ ) {
            UINT32 v;
            UINT32 prevU;       /* previous u */

            prevU = u;

            /* (uv) = a[j] * b[i] */
            EscEccWd_Multiply( &u, &v, a->word[ j ], b->word[ i ] );

            /* (uv) = a[j] * b[i] + u */
            EscEccWd_Inc( &u, &v, prevU );

            /* (uv) = c[i+j] + a[j] * b[i] + u */
            EscEccWd_Inc( &u, &v, c->word[ i + j ] );

            /* c[i+j] = v */
            c->word[ i + j ] = v;
        }

        /* 2.3) c[i+RSA_SIZE_WORDS] = u */
        c->word[ i + ( MUL_MAX_WORDS ) ] = u;
    }
#endif
}

/******************************************
 * multiplies two field elements c = a * b
 ******************************************/
static void
EscEccFe_Multiply(
    EscEcc_FieldElementLongT* c,
    const EscEcc_FieldElementT* a,
    const EscEcc_FieldElementT* b )
{
    /* declarations */
    UINT8 i;

#ifdef Esc_HAS_INT64
    /* row-wise multiplication */
    UINT8 j;
    UINT8 min;
    UINT32 carry, t;
    UINT8 b_0, a_0;
    UINT64 uv_t;
    UINT64 uv;
    uv_t = 0U;
    t = 0U;

    for ( i = 0U; i < ( 2U * EscEcc_MAX_WORDS ); i++ ) {
        b_0 = EscEcc_MIN( ( EscEcc_MAX_WORDS - 1U ), i );
        a_0 = i - b_0;
        min = EscEcc_MIN( ( EscEcc_MAX_WORDS - a_0 ), ( b_0 + 1U ) );
        for ( j = 0U; j < min; j++ ) {
            /* a[a_o +j] * b[b_o - j] */
            uv = (UINT64)a->word[ a_0 + j ] * (UINT64)b->word[ b_0 - j ];
            /* sum on uv_t */
            uv_t += uv;
            /* if overflow save carry to t */
            if ( uv_t < uv ) {
                t++;
            }
        }

        /* c[i] = v, v = u, u = t, t = 0 */
        c->word[ i ] = EscEcc_LO64( uv_t );
        carry = EscEcc_HI64( uv_t );
        uv_t = carry;
        uv_t |= (UINT64)( t ) << 32U;
        t = 0U;
    }

#else

    /* 1.) c = 0 */
    for ( i = 0U; i < EscEcc_MAX_LONG_WORDS; i++ ) {
        c->word[ i ] = 0U;
    }
    /* 2.) for i = 0 to EscEcc_MAX_WORDS */
    for ( i = 0U; i < EscEcc_MAX_WORDS; i++ ) {
        UINT8 j;
        UINT32 u;
        /* 2.1) u = 0 */
        u = 0U;

        /* 2.2) for j = 0 to EscEcc_MAX_WORDS do */
        for ( j = 0U; j < EscEcc_MAX_WORDS; j++ ) {
            UINT32 v;
            UINT32 prevU;       /* previous u */

            prevU = u;

            /* (uv) = a[j] * b[i] */
            EscEccWd_Multiply( &u, &v, a->word[ j ], b->word[ i ] );

            /* (uv) = a[j] * b[i] + u */
            EscEccWd_Inc( &u, &v, prevU );

            /* (uv) = c[i+j] + a[j] * b[i] + u */
            EscEccWd_Inc( &u, &v, c->word[ i + j ] );

            /* c[i+j] = v */
            c->word[ i + j ] = v;
        }

        /* 2.3) c[i+RSA_SIZE_WORDS] = u */
        c->word[ i + EscEcc_MAX_WORDS ] = u;
    }
#endif
}

/******************************************
 * add two field elements under modulus p *
 ******************************************/
void
EscEccFe_ModularAdd(
    EscEcc_FieldElementT* c,
    const EscEcc_FieldElementT* a,
    const EscEcc_FieldElementT* b,
    const EscEcc_FieldT* fGP )
{
    /* declarations  */
    UINT32 borrow;
    UINT32 carry;
    SINT8 compResult;

    /* 1.) & 2.)                                                */
    /* for i from 0 to t-1 do c[i] = add_with_carry(a[i], b[i]) */
    /* -> assembler optimized add loop                          */
    EscEccWd_AddCLoop( &carry, c->word, a->word, b->word );

    /* 3.) if carry bit is set, then subtract p from c = {c[t-1],..,c[1],c[0]} */
    /* The two added FieldElements have a size of EscEcc_KEY_WORDS + 1U (= EscEcc_MAX_WORD).
       Only the barrett reduction uses the most significant 32 bits, and this ModularAdd
       function is not called in the barret reduction. Therefore the most significant 32 bits
       of the added elements are always zero and the if-clause will never become true!
       Leave this for maintainability and possible changes in the barret reduction */
    if ( carry == 1U ) {
        /* for i from 0 to t-1 do c[i] = subtract_with_borrow(c[i], p[i])  */
        /* -> assembler optimized subtract loop */
        EscEccWd_SubBLoop( c->word, &borrow, c->word, fGP->prime_p.word );
    }

    /* 4.) if c >= p, then c = c - p */
    compResult = EscEccFe_AbsoluteCompare( c, &fGP->prime_p );

    if ( compResult == 1 ) {
        /* for i from 0 to t-1 do c[i] = subtract_with_borrow(c[i], p[i])  */
        /* -> assembler optimized subtract loop                            */
        EscEccWd_SubBLoop( c->word, &borrow, c->word, fGP->prime_p.word );
    } else if ( compResult == 0 ) {
        EscEccFe_SetZero( c );
    } else {
        /* compResult == -1 */
        /* do nothing */
    }

    /* sign (always +) */
}

/************************************************
 * subtracts two field elements under modulus p *
 ************************************************/
void
EscEccFe_ModularSub(
    EscEcc_FieldElementT* c,
    const EscEcc_FieldElementT* a,
    const EscEcc_FieldElementT* b,
    const EscEcc_FieldT* fGP )
{
    /* declarations  */
    UINT32 borrow;
    UINT32 carry;

    /* 1.) & 2.) */
    /* for i from 0 to t-1 do c[i] = subtract_with_borrow(a[i], b[i])  */
    /* -> assembler optimized subtract loop (borrow_in = 1)            */
    EscEccWd_SubBLoop( c->word, &borrow, a->word, b->word );

    /* 3.) if carry bit is set (i.e. borrow == 0), then add p to c = {c[t-1],..,c[1],c[0]} */
    if ( borrow == 0U ) {
        /* for i from 0 to t-1 do c[i] = add_with_carry(c[i], p[i]) */
        /* -> assembler optimized add loop (carry_in = 0) */
        EscEccWd_AddCLoop( &carry, c->word, c->word, fGP->prime_p.word );
    }

    /* sign (always +) */
}

/********************************************************
 * reduce a field element z with modulus p using Barrett*
 * r = a mod p                                          *
 ********************************************************/
static void
EscEccFe_ReduceBarrett(
    EscEcc_FieldElementT* r,
    const EscEcc_FieldElementLongT* z,
    const EscEcc_FieldT* fGP )
{
    /* taken from Hankerson, et. al, "Guide to Ell. Curve Crypto.", Algorithm 2.14, Barret Reduction */
    UINT8 i;
    UINT32 borrow;

    EscEcc_FieldElementT tmp;
    EscEcc_FieldElementLongT tmpLong;
    EscEcc_FieldElementT q;
    EscEcc_FieldElementT r1, r2;

    /* reduction */
    /* 1.) q = floor(floor(z / (b ^(k-1))) * mu / b^(k+1)) */

    /*lint -save -e772 variable tmp, r1, r2 have not to be initialized */

    /* tmp = floor(z / (b ^(k-1))) */
    for ( i = 0U; i < ( fGP->pLen + 1U ); i++ ) {
        tmp.word[ i ] = z->word[ ( fGP->pLen - 1U ) + i ];
    }
    for ( i = ( fGP->pLen + 1U ); i < EscEcc_MAX_WORDS; i++ ) {
        tmp.word[ i ] = 0U;
    }

    /* tmpLong = tmp * my_p  = floor(z / (b ^(k-1))) * mu */
    /* use Big-version here because precalc_my_p has full word size */
    EscEccFe_Multiply( &tmpLong, &tmp, &fGP->precalc_my_p );

    /* q = floor(tmpLong / b^(k+1)) = floor(floor(z / (b ^(k-1))) * mu / b^(k+1)) */
    Esc_ASSERT( ( ( fGP->pLen + 1U ) + EscEcc_MAX_WORDS ) <= EscEcc_MAX_LONG_WORDS );
    for ( i = 0U; i < EscEcc_MAX_WORDS; i++ ) {
        q.word[ i ] = tmpLong.word[ ( fGP->pLen + 1U ) + i ];
    }

    /* 2.) r = z % (b^(k+1)) - ((q * p) % (b^(k+1)) */

    /* r1 = (z % (b^(k+1)) */
    for ( i = 0U; i < ( fGP->pLen + 1U ); i++ ) {
        r1.word[ i ] = z->word[ i ];
    }
    /* ReduceBarrett is only called with fGP = 'base_point_order_n' or 
       fGP = 'EscEcc_PRIME_P'(defined ecc_curves32.c) as the modulo.
       Since these fields have a length of EscEcc_MAX_WORDS-1, this loop is never be used!
       Leave this for maintainability and possible changes in future ECC versions */
    for ( i = ( fGP->pLen + 1U ); i < EscEcc_MAX_WORDS; i++ ) {
        r1.word[ i ] = 0U;
    }

    /* tmpLong = q * p */
    EscEccFe_MultiplyShort( &tmpLong, &q, &fGP->prime_p );

    /* r2 = tmpLong % (b^(k+1)) = (q * p) % (b^(k+1)) */
    for ( i = 0U; i < ( fGP->pLen + 1U ); i++ ) {
        r2.word[ i ] = tmpLong.word[ i ];
    }
    /* ReduceBarrett is only called with fGP = 'base_point_order_n' or 
       fGP = 'EscEcc_PRIME_P'(defined ecc_curves32.c) as the modulo.
       Since these fields have a length of EscEcc_MAX_WORDS-1, this loop is never be used!
       Leave this for maintainability and possible changes in future ECC versions */
    for ( i = ( fGP->pLen + 1U ); i < EscEcc_MAX_WORDS; i++ ) {
        r2.word[ i ] = 0U;
    }

    /*r = r1 - r2 = (z % (b^(k+1)) - ((q * p) % (b^(k+1))) */
    if ( EscEccFe_AbsoluteCompare( &r1, &r2 ) < 0 ) {
        /* Include Step 3. if r<0, r := r + b^(k+1) */
        Esc_ASSERT( ( fGP->pLen + 1U ) < EscEcc_MAX_WORDS );
        r1.word[ fGP->pLen + 1U ] = 1U;   /* r1 := r1 + b^(k+1) */
    }
    EscEccWd_SubBLoop( r->word, &borrow, r1.word, r2.word );

    /* 4.) while r>=p do : r:= r - p */
    while ( EscEccFe_AbsoluteCompare( r, &fGP->prime_p ) >= 0 ) {
        /* r := r - p */
        EscEccWd_SubBLoop( r->word, &borrow, r->word, fGP->prime_p.word );
    }
    /* 5.) return r */

    /*lint -restore */
}

/************************************************
 * multiply two field elements under modulus p *
 ************************************************/
void
EscEccFe_ModularMultiply(
    EscEcc_FieldElementT* c,
    const EscEcc_FieldElementT* a,
    const EscEcc_FieldElementT* b,
    const EscEcc_FieldT* fGP )
{
    EscEcc_FieldElementLongT longC;

    /* multiply */
    EscEccFe_MultiplyShort( &longC, a, b );

    /* reduce */
    EscEccFe_Reduce(c, &longC, fGP);
}

/********************************************************
 * reverses a field element with modulus p (inversion)  *
 * applying the Binary Extended Euclidean algorithm     *
 ********************************************************/
void
EscEccFe_ModularInvert(
    EscEcc_FieldElementT* c,
    const EscEcc_FieldElementT* a,
    const EscEcc_FieldT* fGP )
{
    EscEcc_FieldElementT u;
    EscEcc_FieldElementT v;
    EscEcc_FieldElementT x1;
    EscEcc_FieldElementT x2;
    UINT32 carry;
    BOOL isFinished;

    EscEccFe_Assign( &u, a );
    EscEccFe_Assign( &v, &fGP->prime_p );
    EscEccFe_SetZero( &x2 );
    EscEccFe_SetOne( &x1 );

    isFinished = FALSE;

    if ( EscEccFe_IsOne( &u ) ) {
        isFinished = TRUE;
    } else if ( EscEccFe_IsOne( &v ) ) {
        isFinished = TRUE;
    } else {
        /* nothing */
    }

    while ( isFinished == FALSE ) {
        while ( ( u.word[ 0 ] & 1U ) == 0U ) {
            EscEccFe_ShiftRight( &u );
            if ( ( x1.word[ 0 ] & 1U ) == 0U ) {
                EscEccFe_ShiftRight( &x1 );
            } else {
                EscEccWd_AddCLoop( &carry, x1.word, x1.word, fGP->prime_p.word );
                EscEccFe_ShiftRight( &x1 );
            }
        }

        while ( ( v.word[ 0 ] & 1U ) == 0U ) {
            EscEccFe_ShiftRight( &v );
            if ( ( x2.word[ 0 ] & 1U ) == 0U ) {
                EscEccFe_ShiftRight( &x2 );
            } else {
                EscEccWd_AddCLoop( &carry, x2.word, x2.word, fGP->prime_p.word );
                EscEccFe_ShiftRight( &x2 );
            }
        }

        if ( EscEccFe_AbsoluteCompare( &u, &v ) > -1 ) {
            EscEccWd_SubBLoop( u.word, &carry, u.word, v.word );
            EscEccFe_ModularSub( &x1, &x1, &x2, fGP );
        } else {
            EscEccWd_SubBLoop( v.word, &carry, v.word, u.word );
            EscEccFe_ModularSub( &x2, &x2, &x1, fGP );
        }

        if ( EscEccFe_IsOne( &u ) ) {
            isFinished = TRUE;
        } else if ( EscEccFe_IsOne( &v ) ) {
            isFinished = TRUE;
        } else {
            /* nothing */
        }
    }

    if ( EscEccFe_IsOne( &u ) ) {
        EscEccFe_Assign( c, &x1 );
    } else {
        EscEccFe_Assign( c, &x2 );
    }
}

#if ( EscEcc_CURVE_TYPE == EscEcc_secPr1 )
/***************************************
 * optimized reduction for NIST-Primes *
 ***************************************/
#   if ( EscEcc_KEY_LENGTH == 160U )
static void
EscEccFe_ReduceNIST_160(
    EscEcc_FieldElementT* r,
    const EscEcc_FieldElementLongT* z,
    const EscEcc_FieldT* fGP )
{
    /* see da_uhsadel.pdf for details */
    SINT8 compResult;
    UINT8 c_array_word[ 5 ];
    UINT32 tmp;
    UINT8 i;

    /* clear result variable */
    for ( i = 5U; i < EscEcc_MAX_WORDS; i++ ) {
        r->word[ i ] = 0U;
    }

    /* copy lower words of z, add higher words of z and save carrys for each word */
    for ( i = 0U; i < 5U; i++ ) {
        c_array_word[ i ] = 0U;
        r->word[ i ] = z->word[ i ];
        r->word[ i ] += z->word[ i + 5U ];
        if ( r->word[ i ] < z->word[ i + 5U ] ) {
            c_array_word[ i ]++;
        }
    }

    /* add shifted higher words with bit 0 of the next word */
    /* word[1] - word[4] */
    for ( i = 1U; i < 5U; i++ ) {
        tmp = ( z->word[ i + 4U ] >> 1U ) | ( ( z->word[ i + 5U ] & 1U ) << 31U );
        r->word[ i ] += tmp;
        if ( r->word[ i ] < tmp ) {
            c_array_word[ i ]++;
        }
    }

    /* remaining special cases for word[0] and word[1] */
    /* word[0] */
    tmp = ( z->word[ 9 ] >> 1U ) | ( ( ( z->word[ 9 ] >> 1U ) & 1U ) << 31U );
    r->word[ 0 ] += tmp;
    if ( r->word[ 0 ] < tmp ) {
        c_array_word[ 0 ]++;
    }
    /* word[0] */
    tmp = ( z->word[ 5 ] & 1U ) << 31U;
    r->word[ 0 ] += tmp;
    if ( r->word[ 0 ] < tmp ) {
        c_array_word[ 0 ]++;
    }
    /* word[1] */
    tmp = ( z->word[ 9 ] >> 2U );
    r->word[ 1 ] += tmp;
    if ( r->word[ 1 ] < tmp ) {
        c_array_word[ 1 ]++;
    }

    /* add collected carrys */
    for ( i = 0U; i < 4U; i++ ) {
        r->word[ i + 1U ] += c_array_word[ i ];
        if ( r->word[ i + 1U ] < c_array_word[ i ] ) {
            c_array_word[ i + 1U ]++;
        }
    }

    /* save last carry */
    r->word[ 5 ] = c_array_word[ 4 ];

    /* substract modulus if result is bigger */
    do {
        compResult = EscEccFe_AbsoluteCompare( r, &fGP->prime_p );

        if ( compResult == 1 ) {
            EscEccWd_SubBLoop( r->word, &tmp, r->word, fGP->prime_p.word );
        } else if ( compResult == 0 ) {
            EscEccFe_SetZero( r );
        } else {
            /* do nothing */
        }
    } while ( compResult == 1 );
}

#   endif

#   if ( EscEcc_KEY_LENGTH == 192U )
static void
EscEccFe_ReduceNIST_192(
    EscEcc_FieldElementT* r,
    const EscEcc_FieldElementLongT* z,
    const EscEcc_FieldT* fGP )
{
    /* c_x = 2^64
       s1 = (c_2, c_1, c_0) s2 = (0, c_3, c_3) s3 = (c_4, c_4, 0) s4 = c_5, c_5, c_5)
       calculate (s1 + s2 + s3 + s4) mod p_192 */
    UINT16 i;
    EscEcc_FieldElementT s2;
    EscEcc_FieldElementT s3;
    EscEcc_FieldElementT s4;

    EscEccFe_SetZero( &s2 );
    EscEccFe_SetZero( &s3 );
    s4.word[ EscEcc_MAX_WORDS - 1U ] = 0U;
    r->word[ EscEcc_MAX_WORDS - 1U ] = 0U;

    for ( i = 0U; i < 6U; i++ ) {
        r->word[ i ] = z->word[ i ];
    }
    for ( i = 6U; i < 8U; i++ ) {
        s2.word[ i - 6U ] = z->word[ i ];
        s2.word[ i - 4U ] = z->word[ i ];
    }
    for ( i = 8U; i < 10U; i++ ) {
        s3.word[ i - 6U ] = z->word[ i ];
        s3.word[ i - 4U ] = z->word[ i ];
    }
    for ( i = 10U; i < 12U; i++ ) {
        s4.word[ i - 10U ] = z->word[ i ];
        s4.word[ i - 8U ] = z->word[ i ];
        s4.word[ i - 6U ] = z->word[ i ];
    }

    EscEccFe_ModularAdd( r, r, &s2, fGP );
    EscEccFe_ModularAdd( r, r, &s3, fGP );
    EscEccFe_ModularAdd( r, r, &s4, fGP );
}

#   endif

#   if ( EscEcc_KEY_LENGTH == 224U )
static void
EscEccFe_ReduceNIST_224(
    EscEcc_FieldElementT* r,
    const EscEcc_FieldElementLongT* z,
    const EscEcc_FieldT* fGP )
{
    /* Algorithm 2.28, Guide to Elliptic Curve Cryptography */
    UINT16 i;
    EscEcc_FieldElementT s2;
    EscEcc_FieldElementT s3;
    EscEcc_FieldElementT s4;
    EscEcc_FieldElementT s5;

    EscEccFe_SetZero( &s2 );
    EscEccFe_SetZero( &s3 );
    EscEccFe_SetZero( &s5 );
    r->word[ EscEcc_MAX_WORDS - 1U ] = 0U;
    s4.word[ EscEcc_MAX_WORDS - 1U ] = 0U;

    for ( i = 0U; i < 7U; i++ ) {
        r->word[ i ] = z->word[ i ];
    }
    for ( i = 0U; i < 7U; i++ ) {
        s4.word[ i ] = z->word[ i + 7U ];
    }
    for ( i = 3U; i < 7U; i++ ) {
        s2.word[ i ] = z->word[ i + 4U ];
    }
    for ( i = 0U; i < 3U; i++ ) {
        s5.word[ i ] = z->word[ i + 11U ];
        s3.word[ i + 3U ] = z->word[ i + 11U ];
    }

    EscEccFe_ModularAdd( r, r, &s2, fGP );
    EscEccFe_ModularAdd( r, r, &s3, fGP );
    EscEccFe_ModularSub( r, r, &s4, fGP );
    EscEccFe_ModularSub( r, r, &s5, fGP );
}

#   endif

#   if ( EscEcc_KEY_LENGTH == 256U )
static void
EscEccFe_ReduceNIST_256(
    EscEcc_FieldElementT* r,
    const EscEcc_FieldElementLongT* z,
    const EscEcc_FieldT* fGP )
{
    /* c = 2^32
       Algorithm 2.29, Guide to Elliptic Curve Cryptography */
    EscEcc_FieldElementT s2;
    EscEcc_FieldElementT s3;
    EscEcc_FieldElementT s4;
    EscEcc_FieldElementT s5;
    EscEcc_FieldElementT s6;
    EscEcc_FieldElementT s7;
    EscEcc_FieldElementT s8;
    EscEcc_FieldElementT s9;

    UINT16 i;

    r->word[ EscEcc_MAX_WORDS - 1U ] = 0U;
    for ( i = 0U; i < 8U; i++ ) {
        r->word[ i ] = z->word[ i ];
    }
    EscEccFe_SetZero( &s2 );
    for ( i = 3U; i < 8U; i++ ) {
        s2.word[ i ] = z->word[ i + 8U ];
    }
    EscEccFe_SetZero( &s3 );
    for ( i = 3U; i < 7U; i++ ) {
        s3.word[ i ] = z->word[ i + 9U ];
    }
    EscEccFe_SetZero( &s4 );
    for ( i = 0U; i < 2U; i++ ) {
        s4.word[ i ] = z->word[ i + 8U ];
        s4.word[ i + 6U ] = z->word[ i + 14U ];
    }
    s4.word[ 2U ] = z->word[ 10U ];
    s5.word[ EscEcc_MAX_WORDS - 1U ] = 0U;
    for ( i = 0U; i < 3U; i++ ) {
        s5.word[ i ] = z->word[ i + 9U ];
        s5.word[ i + 3U ] = z->word[ i + 13U ];
    }
    s5.word[ 6U ] = z->word[ 13U ];
    s5.word[ 7U ] = z->word[ 8U ];
    EscEccFe_SetZero( &s6 );
    s8.word[ EscEcc_MAX_WORDS - 1U ] = 0U;
    s8.word[ EscEcc_MAX_WORDS - 3U ] = 0U;
    EscEccFe_SetZero( &s9 );
    for ( i = 0U; i < 3U; i++ ) {
        s6.word[ i ] = z->word[ i + 11U ];
        s8.word[ i ] = z->word[ i + 13U ];
        s8.word[ i + 3U ] = z->word[ i + 8U ];
        s9.word[ i + 3U ] = z->word[ i + 9U ];
    }
    s6.word[ 6U ] = z->word[ 8U ];
    s6.word[ 7U ] = z->word[ 10U ];
    s8.word[ 7U ] = z->word[ 12U ];
    s9.word[ 0U ] = z->word[ 14U ];
    s9.word[ 1U ] = z->word[ 15U ];
    s9.word[ 7U ] = z->word[ 13U ];
    EscEccFe_SetZero( &s7 );
    for ( i = 0U; i < 4U; i++ ) {
        s7.word[ i ] = z->word[ i + 12U ];
    }
    s7.word[ 6U ] = z->word[ 9U ];
    s7.word[ 7U ] = z->word[ 11U ];

    EscEccFe_ModularAdd( r, r, &s4, fGP );
    EscEccFe_ModularAdd( &s2, &s2, &s3, fGP );
    EscEccFe_ModularAdd( &s2, &s2, &s2, fGP );
    EscEccFe_ModularAdd( r, r, &s2, fGP );
    EscEccFe_ModularAdd( r, r, &s5, fGP );
    EscEccFe_ModularSub( r, r, &s6, fGP );
    EscEccFe_ModularSub( r, r, &s7, fGP );
    EscEccFe_ModularSub( r, r, &s8, fGP );
    EscEccFe_ModularSub( r, r, &s9, fGP );
}

#   endif

#   if ( EscEcc_KEY_LENGTH == 384U )
static void
EscEccFe_ReduceNIST_384(
    EscEcc_FieldElementT* r,
    const EscEcc_FieldElementLongT* z,
    const EscEcc_FieldT* fGP )
{
    /* c = 2^32
       Algorithm 2.30, Guide to Elliptic Curve Cryptography */
    EscEcc_FieldElementT s2;
    EscEcc_FieldElementT s3;
    EscEcc_FieldElementT s4;
    EscEcc_FieldElementT s5;
    EscEcc_FieldElementT s6;
    EscEcc_FieldElementT s7;
    EscEcc_FieldElementT s8;
    EscEcc_FieldElementT s9;
    EscEcc_FieldElementT s10;

    UINT16 i;

    r->word[ EscEcc_MAX_WORDS - 1U ] = 0U;
    for ( i = 0U; i < 12U; i++ ) {
        r->word[ i ] = z->word[ i ];
    }
    EscEccFe_SetZero( &s2 );
    for ( i = 4U; i < 7U; i++ ) {
        s2.word[ i ] = z->word[ i + 17U ];
    }
    s3.word[ EscEcc_MAX_WORDS - 1U ] = 0U;
    for ( i = 0U; i < 12U; i++ ) {
        s3.word[ i ] = z->word[ i + 12U ];
    }
    s4.word[ EscEcc_MAX_WORDS - 1U ] = 0U;
    for ( i = 3U; i < 12U; i++ ) {
        s4.word[ i ] = z->word[ i + 9U ];
    }
    s4.word[ 0U ] = z->word[ 21U ];
    s4.word[ 1U ] = z->word[ 22U ];
    s4.word[ 2U ] = z->word[ 23U ];
    for ( i = 4U; i < 12U; i++ ) {
        s5.word[ i ] = z->word[ i + 8U ];
    }
    s5.word[ 0U ] = 0U;
    s5.word[ 1U ] = z->word[ 23U ];
    s5.word[ 2U ] = 0U;
    s5.word[ 3U ] = z->word[ 20U ];
    s5.word[ EscEcc_MAX_WORDS - 1U ] = 0U;
    EscEccFe_SetZero( &s6 );
    for ( i = 4U; i < 8U; i++ ) {
        s6.word[ i ] = z->word[ i + 16U ];
    }
    EscEccFe_SetZero( &s7 );
    s7.word[ 0U ] = z->word[ 20U ];
    s7.word[ 3U ] = z->word[ 21U ];
    s7.word[ 4U ] = z->word[ 22U ];
    s7.word[ 5U ] = z->word[ 23U ];
    for ( i = 1U; i < 12U; i++ ) {
        s8.word[ i ] = z->word[ i + 11U ];
    }
    s8.word[ 0U ] = z->word[ 23U ];
    s8.word[ EscEcc_MAX_WORDS - 1U ] = 0U;
    EscEccFe_SetZero( &s9 );
    for ( i = 1U; i < 5U; i++ ) {
        s9.word[ i ] = z->word[ i + 19U ];
    }
    EscEccFe_SetZero( &s10 );
    s10.word[ 3U ] = z->word[ 23U ];
    s10.word[ 4U ] = z->word[ 23U ];

    EscEccFe_ModularAdd( r, r, &s2, fGP );
    EscEccFe_ModularAdd( r, r, &s2, fGP );
    EscEccFe_ModularAdd( r, r, &s3, fGP );
    EscEccFe_ModularAdd( r, r, &s4, fGP );
    EscEccFe_ModularAdd( r, r, &s5, fGP );
    EscEccFe_ModularAdd( r, r, &s6, fGP );
    EscEccFe_ModularAdd( r, r, &s7, fGP );
    EscEccFe_ModularSub( r, r, &s8, fGP );
    EscEccFe_ModularSub( r, r, &s9, fGP );
    EscEccFe_ModularSub( r, r, &s10, fGP );
}

#   endif

#   if ( EscEcc_KEY_LENGTH == 521U )
static void
EscEccFe_ReduceNIST_521(
    EscEcc_FieldElementT* r,
    const EscEcc_FieldElementLongT* z,
    const EscEcc_FieldT* fGP )
{
    /* c = 2^1
       Algorithm 2.31, Guide to Elliptic Curve Cryptography */
    EscEcc_FieldElementT s2;

    UINT16 i;

    r->word[ EscEcc_MAX_WORDS - 1U ] = 0U;

    for ( i = 0U; i < 17U; i++ ) {
        r->word[ i ] = (z->word[ i + 16U ] >> 9U) ;
        r->word[ i ] = (r->word[ i ] | (z->word[ i + 17U ] << 23U)) ;
    }

    s2.word[ EscEcc_MAX_WORDS - 1U ] = 0U;

    for ( i = 0U; i < 17U; i++ ) {
        s2.word[ i ] = z->word[ i ];
    }
    /* we want the bits 512 to 520 only */
    s2.word[ 16U ] = (s2.word[ 16U ] & (UINT32)0x1FF);

    EscEccFe_ModularAdd( r, r, &s2, fGP );
}

#   endif /* EscEcc_KEY_LENGTH */
#endif /* EscEcc_CURVE_TYPE */


void
EscEccFe_Reduce(
    EscEcc_FieldElementT* r,
    const EscEcc_FieldElementLongT* z,
    const EscEcc_FieldT* fGP )
{
#if ( EscEcc_CURVE_TYPE == EscEcc_secPr1 )
#   if ( EscEcc_KEY_LENGTH == 160U )
    if ( EscEccFe_AbsoluteCompare( &fGP->prime_p, &EscEcc_curve32.ecc_field_params.prime_p ) == 0 ) {
        EscEccFe_ReduceNIST_160( r, z, fGP );
    } else {
        EscEccFe_ReduceBarrett( r, z, fGP );
    }
#   elif ( EscEcc_KEY_LENGTH == 192U )
    if ( EscEccFe_AbsoluteCompare( &fGP->prime_p, &EscEcc_curve32.ecc_field_params.prime_p ) == 0 ) {
        EscEccFe_ReduceNIST_192( r, z, fGP );
    } else {
        EscEccFe_ReduceBarrett( r, z, fGP );
    }
#   elif ( EscEcc_KEY_LENGTH == 224U )
    if ( EscEccFe_AbsoluteCompare( &fGP->prime_p, &EscEcc_curve32.ecc_field_params.prime_p ) == 0 ) {
        EscEccFe_ReduceNIST_224( r, z, fGP );
    } else {
        EscEccFe_ReduceBarrett( r, z, fGP );
    }
#   elif ( EscEcc_KEY_LENGTH == 256U )
    if ( EscEccFe_AbsoluteCompare( &fGP->prime_p, &EscEcc_curve32.ecc_field_params.prime_p ) == 0 ) {
        EscEccFe_ReduceNIST_256( r, z, fGP );
    } else {
        EscEccFe_ReduceBarrett( r, z, fGP );
    }

#   elif ( EscEcc_KEY_LENGTH == 384U )
    if ( EscEccFe_AbsoluteCompare( &fGP->prime_p, &EscEcc_curve32.ecc_field_params.prime_p ) == 0 ) {
        EscEccFe_ReduceNIST_384( r, z, fGP );
    } else {
        EscEccFe_ReduceBarrett( r, z, fGP );
    }
#   elif ( EscEcc_KEY_LENGTH == 521U )
    if ( EscEccFe_AbsoluteCompare( &fGP->prime_p, &EscEcc_curve32.ecc_field_params.prime_p ) == 0 ) {
        EscEccFe_ReduceNIST_521( r, z, fGP );
    } else {
        EscEccFe_ReduceBarrett( r, z, fGP );
    }
#   else
    EscEccFe_ReduceBarrett( r, z, fGP );
#   endif /* EscEcc_KEY_LENGTH  */
#else
    EscEccFe_ReduceBarrett( r, z, fGP );
#endif /* EscEcc_CURVE_TYPE */
}

/*************************************************
 * return true if bit on position "index" is set *
 *************************************************/
static BOOL
EscEccFe_isBitSet(
    const EscEcc_FieldElementT* c,
    SINT16 index )
{
    BOOL isSet;
    if ( ( ( c->word[ (UINT16)index >> 5U ] >> ( (UINT16)index & 0x1fU ) ) & 1U ) == 1U ) {
        isSet = TRUE;
    } else {
        isSet = FALSE;
    }

    return isSet;
}


void
EscEccPt_ToUint8(
    const EscEcc_PointT* input,  /* point to convert */
    UINT8 output[] )            /* result                   */
{
	EscEccFe_ToUint8(&input->x, &output[0U]);
	EscEccFe_ToUint8(&input->y, &output[ECC_KEY_BYTES]);
	EscEccFe_ToUint8(&input->z, &output[ECC_KEY_BYTES * 2U]);
}

void
EscEccPt_FromUint8(
    const UINT8 input[],        /* UINT8 array to convert           */
    EscEcc_PointT* output )
{
	EscEccFe_FromUint8(&input[0U], &output->x);
	EscEccFe_FromUint8(&input[ECC_KEY_BYTES], &output->y);
	EscEccFe_FromUint8(&input[ECC_KEY_BYTES * 2U], &output->z);

	output->type = C_PT_JACOBIAN;
}



/**************************
 * sets point to infinity *
 **************************/
void
EscEccPt_SetInfinity(
    EscEcc_PointT* pX )
{
    if ( pX->type == C_PT_AFFINE ) {
        /* pX = (0,0,0) */
        EscEccFe_SetZero( &pX->x );
        EscEccFe_SetZero( &pX->y );
        EscEccFe_SetZero( &pX->z );
    } else {
        /* pX->type == C_PT_JACOBIAN */
        /* pX = (1,1,0) */
        EscEccFe_SetOne( &pX->x );
        EscEccFe_SetOne( &pX->y );
        EscEccFe_SetZero( &pX->z );
    }
}

/*****************************
 * checks if a point is zero *
 *****************************/
static BOOL
EscEccPt_IsZero(
    const EscEcc_PointT* pX )
{
    BOOL isZero = TRUE;

    if ( EscEccFe_IsZero( &pX->x ) == FALSE ) {
        isZero = FALSE;
    } else if ( EscEccFe_IsZero( &pX->y ) == FALSE ) {
        isZero = FALSE;
    } else {
        if ( ( pX->type == C_PT_JACOBIAN ) ) {
            if ( EscEccFe_IsZero( &pX->z ) == FALSE ) {
                isZero = FALSE;
            }
        }
    }

    return isZero;
}

/***********************************
 * check if a point is at infinity *
 ***********************************/
BOOL
EscEccPt_IsInfinity(
    const EscEcc_PointT* pX )
{
    BOOL isInfinity;

    /* check if infinity */
    if ( pX->type == C_PT_AFFINE ) {
        /* pX =? (0,0) */
        isInfinity = EscEccPt_IsZero( pX );
    } else {
        /* pX->type == C_PT_JACOBIAN */
        /* pX =? (lamba²,lamba³,0) */
        isInfinity = EscEccFe_IsZero( &pX->z );
    }

    return isInfinity;
}


/****************************************************
 * check if a point is a correct point on the curve *
 ****************************************************/
BOOL EscEccPt_IsPoint(
    const EscEcc_PointT* pX )
{
	BOOL isPoint = FALSE;
	EscEcc_PointT pA;
	EscEcc_FieldElementT tmp1, tmp2;

	/* check if pX is affine */
	if (pX->type != C_PT_AFFINE) {
		EscEccPt_ToAffine(&pA, pX);
	} else {
		EscEccPt_Assign(&pA, pX);
	}

	/*** check if EC equation y^2 = x^3 + ax + b holds ***/
	/* tmp1 = ax */
	EscEccFe_ModularMultiply( &tmp1, &EscEcc_curve32.coefficient_a , &pA.x, &EscEcc_curve32.ecc_field_params );
	/* tmp1 = ax + b */
	EscEccFe_ModularAdd( &tmp1, &tmp1, &EscEcc_curve32.coefficient_b, &EscEcc_curve32.ecc_field_params );
	/* tmp2 = x^3 */
	EscEccFe_ModularMultiply( &tmp2, &pA.x , &pA.x, &EscEcc_curve32.ecc_field_params );
	EscEccFe_ModularMultiply( &tmp2, &tmp2 , &pA.x, &EscEcc_curve32.ecc_field_params );
	/* tmp1 = x^3 + ax + b */
	EscEccFe_ModularAdd( &tmp1, &tmp2, &tmp1, &EscEcc_curve32.ecc_field_params );
	/* tmp2 = y^2 */
	EscEccFe_ModularMultiply( &tmp2, &pA.y , &pA.y, &EscEcc_curve32.ecc_field_params );

	/* check for equality */
	if (EscEccFe_AbsoluteCompare(&tmp1, &tmp2) == 0) {
		isPoint = TRUE;
	}

	return isPoint;
}

/****************************************
 * returns TRUE if points are equal. 	*
 ****************************************/
BOOL
EscEccPt_IsEqual(
	const EscEcc_PointT* pA0,
	const EscEcc_PointT* pA1)
{
	EscEcc_PointT pJ0, pJ1;
	EscEcc_PointT pJ1_neg;
	EscEcc_FieldElementT y_neg;
	BOOL result = FALSE;

    if (pA0->type == C_PT_AFFINE) {
		pJ0.type = C_PT_JACOBIAN;
		EscEccFe_Assign(&pJ0.x, &pA0->x);
		EscEccFe_Assign(&pJ0.y, &pA0->y);
		EscEccFe_SetOne(&pJ0.z);
    } else {
        EscEccPt_Assign(&pJ0, pA0);
    }

    if (pA1->type == C_PT_AFFINE) {
    	pJ1.type = C_PT_JACOBIAN;
		EscEccFe_Assign(&pJ1.x, &pA1->x);
		EscEccFe_Assign(&pJ1.y, &pA1->y);
		EscEccFe_SetOne(&pJ1.z);
    } else {
        EscEccPt_Assign(&pJ1, pA1);
    }

    /* absolute compare of each coordinate */
    if ((EscEccFe_AbsoluteCompare(&pJ0.x, &pJ1.x)==0) &&
    	(EscEccFe_AbsoluteCompare(&pJ0.y, &pJ1.y)==0) &&
    	(EscEccFe_AbsoluteCompare(&pJ0.z, &pJ1.z)==0)) {
    	result = TRUE;
    }

    /* relative compare -> compute P0+(-P1) and check for infinity */
    else {
    	EscEccFe_SetZero(&y_neg);
        EscEccFe_ModularSub(&y_neg, &y_neg, &pJ1.y, &EscEcc_curve32.ecc_field_params);
        EscEccPt_Assign(&pJ1_neg, &pJ1);
        EscEccFe_Assign(&pJ1_neg.y, &y_neg);
        EscEccPt_JacobianAdd(&pJ1_neg, &pJ0, &pJ1_neg);
        result = EscEccPt_IsInfinity(&pJ1_neg);
    }

    return result;
}

/******************
 * assign a point *
 ******************/
void
EscEccPt_Assign(
    EscEcc_PointT* pY,
    const EscEcc_PointT* pX )
{
    /* assign type */
    pY->type = pX->type;

    /* assign coordinates */
    EscEccFe_Assign( &pY->x, &pX->x );
    EscEccFe_Assign( &pY->y, &pX->y );
    EscEccFe_Assign( &pY->z, &pX->z );
}


BOOL
EscEccPt_Check(
    const EscEcc_PointT* pQ)
{
    BOOL hasFailed = TRUE;
    SINT8 comp;

    if ( EscEccPt_IsInfinity( pQ ) == FALSE ) {
        /* B.) check that Q(x,y,z) consists of properly represented elements of GF(p) */
        /* 0 < pQ->x < p */
        comp = EscEccFe_AbsoluteCompare( &pQ->x, &EscEcc_curve32.ecc_field_params.prime_p );
        if ( comp == -1 ) {
            /* 0 < pQ->y < p */
            comp = EscEccFe_AbsoluteCompare( &pQ->y, &EscEcc_curve32.ecc_field_params.prime_p );
            if ( (pQ->type == C_PT_JACOBIAN) && ( comp == -1 ) ) {
                /* 0 < pQ->z < p */
                comp = EscEccFe_AbsoluteCompare( &pQ->z, &EscEcc_curve32.ecc_field_params.prime_p );
                if ( comp == -1 ) {
                    hasFailed = FALSE;
                }
            }
            else {
                if ( comp == -1 ) {
                    hasFailed = FALSE;
                }
            }
        }
    }

    return hasFailed;
}

/****************************************
 * convert a point into an affine point *
 ****************************************/
void
EscEccPt_ToAffine(
    EscEcc_PointT* pA,
    const EscEcc_PointT* pJ )
{
    if ( pJ->type == C_PT_JACOBIAN ) {
		if ( EscEccPt_IsInfinity( pJ ) ) {
			/* pA = (0) */
			pA->type = C_PT_AFFINE;
			EscEccPt_SetInfinity( pA );
		} else {
			EscEcc_FieldElementT fe_z_inv1; /* pJ(1/z) */
			EscEcc_FieldElementT fe_z_inv2; /* pJ(1/z^2) */
			EscEcc_FieldElementT fe_z_inv3; /* pJ(1/z^3) */

			/* set pA type */
			pA->type = C_PT_AFFINE;

			/* pJ(1/z) : 1I */
			EscEccFe_ModularInvert( &fe_z_inv1, &pJ->z, &EscEcc_curve32.ecc_field_params );
			/* pJ(1/z²) : 1S */
			EscEccFe_ModularMultiply( &fe_z_inv2, &fe_z_inv1, &fe_z_inv1, &EscEcc_curve32.ecc_field_params );
			/* pJ(1/z³) : 1M */
			EscEccFe_ModularMultiply( &fe_z_inv3, &fe_z_inv1, &fe_z_inv2, &EscEcc_curve32.ecc_field_params );
			/* pA(x) = pJ(x/z²) : 1M */
			EscEccFe_ModularMultiply( &pA->x, &pJ->x, &fe_z_inv2, &EscEcc_curve32.ecc_field_params );
			/* pA(y) = pJ(y/z³) : 1M */
			EscEccFe_ModularMultiply( &pA->y, &pJ->y, &fe_z_inv3, &EscEcc_curve32.ecc_field_params );
			/*pA(z) = 0 */
			EscEccFe_SetZero( &pA->z );
		}
    }
}

/***************************************
 * doubles a jacobian projection point *
 ***************************************/
static void
EscEccPt_JacobianDouble(
    EscEcc_PointT* pJ,
    const EscEcc_PointT* pJ0 )
{
    Esc_ASSERT( pJ0->type == C_PT_JACOBIAN );

    /* set pJ type */
    pJ->type = C_PT_JACOBIAN;

    /* check for point at infinity */
    if ( EscEccPt_IsInfinity( pJ0 ) ) {
        EscEccPt_SetInfinity( pJ );
    } else if ( EscEccFe_IsZero( &pJ0->y ) ) {
        EscEccPt_SetInfinity( pJ );
    } else if ( EscEccFe_IsZero( &pJ0->z ) ) {
        EscEccPt_SetInfinity( pJ );
    } else {
        EscEcc_FieldElementT T1;
        EscEcc_FieldElementT T2;
        EscEcc_FieldElementT T3;
        EscEcc_FieldElementT T4;
        EscEcc_FieldElementT T5;
        EscEcc_FieldElementT TF;

        /* 01.) T1 <- X1 */
        EscEccFe_Assign( &T1, &pJ0->x );
        /* 02.) T2 <- Y1 */
        EscEccFe_Assign( &T2, &pJ0->y );
        /* 03.) T3 <- Z1 */
        EscEccFe_Assign( &T3, &pJ0->z );

        /* 04.) If T2 = 0 or T3 = 0 then output (1, 1, 0) and stop */
        /* checked above */

        /*        T4 <- a */
        EscEccFe_Assign( &T4, &EscEcc_curve32.coefficient_a );
        /*        T5 <- T3² */
        EscEccFe_ModularMultiply( &T5, &T3, &T3, &EscEcc_curve32.ecc_field_params );
        /*        T5 <- T5² */
        EscEccFe_ModularMultiply( &T5, &T5, &T5, &EscEcc_curve32.ecc_field_params );
        /*        T5 <- T4 × T5 */
        EscEccFe_ModularMultiply( &T5, &T4, &T5, &EscEcc_curve32.ecc_field_params );
        /*        T4 <- T1² */
        EscEccFe_ModularMultiply( &T4, &T1, &T1, &EscEcc_curve32.ecc_field_params );
        /*        T4 <- 3 × T4 */
        EscEccFe_Assign( &TF, &T4 );
        EscEccFe_ModularAdd( &T4, &T4, &T4, &EscEcc_curve32.ecc_field_params );
        EscEccFe_ModularAdd( &T4, &T4, &TF, &EscEcc_curve32.ecc_field_params );

        /*        T4 <- T4 + T5 = M */
        EscEccFe_ModularAdd( &T4, &T4, &T5, &EscEcc_curve32.ecc_field_params );
        /* 06.) T3 <- T2 × T3 */
        EscEccFe_ModularMultiply( &T3, &T2, &T3, &EscEcc_curve32.ecc_field_params );
        /* 07.) T3 <- 2 × T3 = Z2 */
        EscEccFe_ModularAdd( &T3, &T3, &T3, &EscEcc_curve32.ecc_field_params );
        /* 08.) T2 <- T2² */
        EscEccFe_ModularMultiply( &T2, &T2, &T2, &EscEcc_curve32.ecc_field_params );
        /* 09.) T5 <- T1 × T2 */
        EscEccFe_ModularMultiply( &T5, &T1, &T2, &EscEcc_curve32.ecc_field_params );
        /* 10.) T5 <- 4 × T5 = S */
        EscEccFe_ModularAdd( &T5, &T5, &T5, &EscEcc_curve32.ecc_field_params );
        EscEccFe_ModularAdd( &T5, &T5, &T5, &EscEcc_curve32.ecc_field_params );
        /* 11.) T1 <- T4² */
        EscEccFe_ModularMultiply( &T1, &T4, &T4, &EscEcc_curve32.ecc_field_params );
        /* 12.) T1 <- T1 - 2 × T5 = X2 */
        EscEccFe_ModularSub( &T1, &T1, &T5, &EscEcc_curve32.ecc_field_params );
        EscEccFe_ModularSub( &T1, &T1, &T5, &EscEcc_curve32.ecc_field_params );
        /* 13.) T2 <- T2² */
        EscEccFe_ModularMultiply( &T2, &T2, &T2, &EscEcc_curve32.ecc_field_params );
        /* 14.) T2 <- 8 × T2 = T */
        EscEccFe_ModularAdd( &T2, &T2, &T2, &EscEcc_curve32.ecc_field_params );
        EscEccFe_ModularAdd( &T2, &T2, &T2, &EscEcc_curve32.ecc_field_params );
        EscEccFe_ModularAdd( &T2, &T2, &T2, &EscEcc_curve32.ecc_field_params );
        /* 15.) T5 <- T5 - T1 */
        EscEccFe_ModularSub( &T5, &T5, &T1, &EscEcc_curve32.ecc_field_params );
        /* 16.) T5 <- T4 × T5 */
        EscEccFe_ModularMultiply( &T5, &T4, &T5, &EscEcc_curve32.ecc_field_params );
        /* 17.) T2 <- T5 - T2 = Y2 */
        EscEccFe_ModularSub( &T2, &T5, &T2, &EscEcc_curve32.ecc_field_params );

        /* 18.) X2 <- T1 */
        EscEccFe_Assign( &pJ->x, &T1 );
        /* 19.) Y2 <- T2 */
        EscEccFe_Assign( &pJ->y, &T2 );
        /* 20.) Z2 <- T3 */
        EscEccFe_Assign( &pJ->z, &T3 );
    }                           /* ! EscEccPt_IsInfinity( pJ0 ) */
}

/**********************************************************
 * add two jacobian points to a jacobian projection point *
 **********************************************************/
void
EscEccPt_JacobianAdd(
    EscEcc_PointT* pJ,
    const EscEcc_PointT* pJ0,
    const EscEcc_PointT* pJ1 )
{
    Esc_ASSERT( pJ0->type == C_PT_JACOBIAN );
    Esc_ASSERT( pJ1->type == C_PT_JACOBIAN );

    /* set pJ type */
    pJ->type = C_PT_JACOBIAN;

    /* pJ0 =? (0) */
    if ( EscEccPt_IsInfinity( pJ0 ) ) {
        /* pJ <- pJ1 */
        EscEccPt_Assign( pJ, pJ1 );
    } else if ( EscEccPt_IsInfinity( pJ1 ) ) {       /* pJ1 =? (0) */
        /* pJ <- pJ0 */
        EscEccPt_Assign( pJ, pJ0 );
    } else {
        BOOL Z1_is_1;

        /* declare buffers */
        EscEcc_FieldElementT T1;
        EscEcc_FieldElementT T2;
        EscEcc_FieldElementT T3;
        EscEcc_FieldElementT T4;
        EscEcc_FieldElementT T5;
        EscEcc_FieldElementT T6;
        EscEcc_FieldElementT T7;

        /* 1.) T1 <- X0 = U0 (if Z1 = 1) */
        EscEccFe_Assign( &T1, &pJ0->x );
        /* 2.) T2 <- Y0 = S0 (if Z1 = 1) */
        EscEccFe_Assign( &T2, &pJ0->y );
        /* 3.) T3 <- Z0 */
        EscEccFe_Assign( &T3, &pJ0->z );
        /* 4.) T4 <- X1 */
        EscEccFe_Assign( &T4, &pJ1->x );
        /* 5.) T5 <- Y1 */
        EscEccFe_Assign( &T5, &pJ1->y );

        /* 6.) if Z1 != 1 then */
        Z1_is_1 = EscEccFe_IsOne( &pJ1->z );
        if ( Z1_is_1 == FALSE ) {
            /* 6.a) T6 <- Z1 */
            EscEccFe_Assign( &T6, &pJ1->z );
            /* 6.b) T7 <- T6² */
            EscEccFe_ModularMultiply( &T7, &T6, &T6, &EscEcc_curve32.ecc_field_params );
            /* 6.c) T1 <- T1 × T7 = U0 (if Z1 != 1) */
            EscEccFe_ModularMultiply( &T1, &T1, &T7, &EscEcc_curve32.ecc_field_params );
            /* 6.d) T7 <- T6 × T7 */
            EscEccFe_ModularMultiply( &T7, &T6, &T7, &EscEcc_curve32.ecc_field_params );
            /* 6.e) T2 <- T2 × T7 = S0 (if Z1 != 1) */
            EscEccFe_ModularMultiply( &T2, &T2, &T7, &EscEcc_curve32.ecc_field_params );
        }

        /* 7.) T7 <- T3² */
        EscEccFe_ModularMultiply( &T7, &T3, &T3, &EscEcc_curve32.ecc_field_params );
        /* 8.) T4 <- T4 × T7 = U1 */
        EscEccFe_ModularMultiply( &T4, &T4, &T7, &EscEcc_curve32.ecc_field_params );
        /* 9.) T7 <- T3 × T7 */
        EscEccFe_ModularMultiply( &T7, &T3, &T7, &EscEcc_curve32.ecc_field_params );
        /* 10.) T5 <- T5 × T7 = S1 */
        EscEccFe_ModularMultiply( &T5, &T5, &T7, &EscEcc_curve32.ecc_field_params );
        /* 11.) T4 <- T1 - T4 = W */
        EscEccFe_ModularSub( &T4, &T1, &T4, &EscEcc_curve32.ecc_field_params );
        /* 12.) T5 <- T2 - T5 = R */
        EscEccFe_ModularSub( &T5, &T2, &T5, &EscEcc_curve32.ecc_field_params );

        /* 13.) if T4 = 0 then */
        if ( EscEccFe_IsZero( &T4 ) ) {
            if ( EscEccFe_IsZero( &T5 ) ) {
                /* 13.a) if T5 = 0 then output (0,0,0) and stop */
                /* double pJ1 */
                EscEccPt_JacobianDouble( pJ, pJ1 );
            } else {
                /* 13.b) else output (1, 1, 0) and stop */
                /* pJ = (0) */
                EscEccPt_SetInfinity( pJ );
            }
        } else {
            UINT32 carry;

            /* 14.) T1 <- 2 × T1 - T4 = T */
            EscEccFe_ModularAdd( &T1, &T1, &T1, &EscEcc_curve32.ecc_field_params );
            EscEccFe_ModularSub( &T1, &T1, &T4, &EscEcc_curve32.ecc_field_params );

            /* 15.) T2 <- 2 × T2 - T5 = M */
            EscEccFe_ModularAdd( &T2, &T2, &T2, &EscEcc_curve32.ecc_field_params );
            EscEccFe_ModularSub( &T2, &T2, &T5, &EscEcc_curve32.ecc_field_params );

            /* 16.) If Z1 != 1 then T3 <- T3 × T6 */
            if ( Z1_is_1 == FALSE ) {
                /*lint -save -e645 T6 is initialized */
                EscEccFe_ModularMultiply( &T3, &T3, &T6, &EscEcc_curve32.ecc_field_params );
                /*lint -restore */
            }

            /* 17.) T3 <- T3 × T4 = Z2 */
            EscEccFe_ModularMultiply( &T3, &T3, &T4, &EscEcc_curve32.ecc_field_params );
            /* 18.) T7 <- T4² */
            EscEccFe_ModularMultiply( &T7, &T4, &T4, &EscEcc_curve32.ecc_field_params );
            /* 19.) T4 <- T4 × T7 */
            EscEccFe_ModularMultiply( &T4, &T4, &T7, &EscEcc_curve32.ecc_field_params );
            /* 20.) T7 <- T1 × T7 */
            EscEccFe_ModularMultiply( &T7, &T1, &T7, &EscEcc_curve32.ecc_field_params );
            /* 21.) T1 <- T5² */
            EscEccFe_ModularMultiply( &T1, &T5, &T5, &EscEcc_curve32.ecc_field_params );
            /* 22.) T1 <- T1 - T7 = X2 */
            EscEccFe_ModularSub( &T1, &T1, &T7, &EscEcc_curve32.ecc_field_params );
            /* 23.) T7 <- T7 - 2 × T1 = V */
            EscEccFe_ModularSub( &T7, &T7, &T1, &EscEcc_curve32.ecc_field_params );
            EscEccFe_ModularSub( &T7, &T7, &T1, &EscEcc_curve32.ecc_field_params );

            /* 24.) T5 <- T5 × T7 */
            EscEccFe_ModularMultiply( &T5, &T5, &T7, &EscEcc_curve32.ecc_field_params );
            /* 25.) T4 <- T2 × T4 */
            EscEccFe_ModularMultiply( &T4, &T2, &T4, &EscEcc_curve32.ecc_field_params );
            /* 26.) T2 <- T5 - T4 */
            EscEccFe_ModularSub( &T2, &T5, &T4, &EscEcc_curve32.ecc_field_params );
            /* 27.) T2 <- T2 / 2 = Y2 */
            /* compute (p+1)/2 = inv(2) */
            EscEccFe_Assign( &T7, &EscEcc_curve32.ecc_field_params.prime_p );
            EscEccFe_SetOne( &T6 );
            EscEccWd_AddCLoop( &carry, T7.word, T7.word, T6.word );
            EscEccFe_ShiftRight( &T7 );
            EscEccFe_ModularMultiply( &T2, &T2, &T7, &EscEcc_curve32.ecc_field_params );

            /* 28.) X2 <- T1 */
            EscEccFe_Assign( &pJ->x, &T1 );
            /* 29.) Y2 <- T2 */
            EscEccFe_Assign( &pJ->y, &T2 );
            /* 30.) Z2 <- T3 */
            EscEccFe_Assign( &pJ->z, &T3 );
        }                       /* !EscEccFe_IsZero( &T4 ) */
    }                           /* pJ0 != (0) && pJ1 != (0) */
}

/************************************************************
 * sub a jacobian point pJ1 from another jacobian point pJ0 *
 ************************************************************/
void
EscEccPt_JacobianSub(
    EscEcc_PointT* pJ,
    const EscEcc_PointT* pJ0,
    const EscEcc_PointT* pJ1 )
{
	EscEcc_PointT pJ1_neg;
	EscEcc_FieldElementT zero;
	EscEcc_FieldElementT y1_neg;

    /* set pJ type */
    pJ->type = C_PT_JACOBIAN;

    /* get negative y coordinate of pJ1 */
    EscEccFe_SetZero(&zero);
    EscEccFe_ModularSub( &y1_neg, &zero, &pJ1->y, &EscEcc_curve32.ecc_field_params );

    /* negative point has same coordinates but with negative y */
    EscEccPt_Assign(&pJ1_neg, pJ1);
    EscEccFe_Assign(&pJ1_neg.y, &y1_neg);

    /* add pJ0 and (- pJ1) to pJ */
    EscEccPt_JacobianAdd(pJ, pJ0, &pJ1_neg);
}

/***********************************************
 * scalar multiplication of unknown pA1 with k *
 ***********************************************/
void
EscEccPt_JacobianMultiplyBinary(
    EscEcc_PointT* pJ,
    const EscEcc_FieldElementT* k,
    const EscEcc_PointT* pA1 )
{
#ifdef EscEcc_USE_SLIDING_WINDOW

    EscEcc_PointT pP;

    if ( pA1->type == C_PT_AFFINE ) {
        pP.type = C_PT_JACOBIAN;
        /* pJx = pXx */
        EscEccFe_Assign( &pP.x, &pA1->x );
        /* pJy = pXy */
        EscEccFe_Assign( &pP.y, &pA1->y );
        /* pJz = 1 */
        EscEccFe_SetOne( &pP.z );
    } else {
        EscEccPt_Assign(&pP, pA1);
    }

    /* set pJ type */
    pJ->type = C_PT_JACOBIAN;
    EscEccPt_SetInfinity( pJ );

    /* check P = (0) */
    if ( EscEccPt_IsZero( pA1 ) == FALSE ) {
        /* pJ = (0) */
        SINT16 i, j, windowsize;
        UINT16 exponent, n;
        /* array w/ size 2^k_m */
        static EscEcc_PointT fe_array[ (UINT16)( (UINT16)( (UINT16)2U << ( EscEcc_WINDOW_SIZE - 1U ) ) + 1U ) ];
        EscEcc_PointT point_2;

        /* precomputation - i < ((2^k_m) / 2) + 1 */
        /* fe_array[0] will not be used!! */
        EscEccPt_Assign( &fe_array[ 1 ], &pP );
        EscEccPt_JacobianDouble( &point_2, &fe_array[ 1 ] );

        for ( i = 2; i < (SINT16)( (UINT16)( (UINT16)2U << ( EscEcc_WINDOW_SIZE - 1U ) ) + 1U ); i++ ) {
            EscEccPt_JacobianAdd( &fe_array[ i ], &fe_array[ i - 1 ], &point_2 );
        }

        i = ( (SINT16)EscEcc_MAX_WORDS * (SINT16)EscEcc_WORD_BITS ) - 1;

        while ( i >= 0 ) {
            /* if Exponent = 0 double only */
            if ( EscEccFe_isBitSet( k, i ) == FALSE ) {
                EscEccPt_JacobianDouble( pJ, pJ );
                i--;
            } else {
                /* build window */
                exponent = 0U;
                n = (UINT16)EscEcc_WINDOW_SIZE - 1U;
                windowsize = (SINT16)EscEcc_WINDOW_SIZE;
                for ( j = i; ( j > ( i - (SINT16)EscEcc_WINDOW_SIZE ) ) && ( j >= 0 ); j-- ) {
                    /* build temporary exponent */
                    if ( EscEccFe_isBitSet( k, j ) == TRUE ) {
                        /*lint -save -e701 n is unsigned*/
                        exponent += (UINT16)( ( (UINT16)1U ) << n );
                        /*lint -restore */
                    }
                    n--;
                }
                i -= (SINT16)EscEcc_WINDOW_SIZE;
                /* make window odd and adjust i, reduce window size */
                while ( ( exponent % 2U ) == 0U ) {
                    exponent = (UINT16)( exponent / 2U );
                    i++;
                    windowsize--;
                }

                /* double */
                for ( j = 0; j < windowsize; j++ ) {
                    EscEccPt_JacobianDouble( pJ, pJ );
                }
                /* add */
                EscEccPt_JacobianAdd( pJ, pJ, &fe_array[ (UINT16)( exponent / 2U ) + 1U ] );
            }
        }
    }
#else

    EscEcc_PointT pP;

    if ( pA1->type == C_PT_AFFINE ) {
		pP.type = C_PT_JACOBIAN;
		/* pJx = pXx */
		EscEccFe_Assign( &pP.x, &pA1->x );
		/* pJy = pXy */
		EscEccFe_Assign( &pP.y, &pA1->y );
		/* pJz = 1 */
		EscEccFe_SetOne( &pP.z );
    } else {
    	EscEccPt_Assign(&pP, pA1);
    }

    /* set pJ type */
    pJ->type = C_PT_JACOBIAN;
    EscEccPt_SetInfinity( pJ );

    /* check P = (0) */
    if ( EscEccPt_IsZero( pA1 ) ) {
        /* pJ = (0) */
        EscEccPt_SetInfinity( pJ );

    } else {
        SINT16 i;

        for ( i = (SINT16)EscEcc_MAX_WORDS - 1; i >= 0; i-- ) {
            UINT16 j;
            UINT32 temp;

            temp = k->word[ i ];

            for ( j = 0U; j < EscEcc_WORD_BITS; j++ ) {
                EscEccPt_JacobianDouble( pJ, pJ );

                if ( ( temp & 0x80000000U ) != 0U ) {      /* highest bit set */
                    EscEccPt_JacobianAdd( pJ, pJ, &pP );
                }
                temp <<= 1;
            }
        }
    }
#endif
}


/*************************************************
 * calculate k0 * G + k1 * Q with Shamir's trick *
 *************************************************/

void
EscEccPt_JacDualMulAddBin(
    EscEcc_PointT* pJ,
    const EscEcc_FieldElementT* k0,
    const EscEcc_PointT* pA0,
    const EscEcc_FieldElementT* k1,
    const EscEcc_PointT* pA1 )
{
    EscEcc_PointT pPA0, pPA1;

    if ( pA0->type == C_PT_AFFINE ) {
        pPA0.type = C_PT_JACOBIAN;
        /* pPAx = pAx */
        EscEccFe_Assign( &pPA0.x, &pA0->x );
        /* pPAy = pAy */
        EscEccFe_Assign( &pPA0.y, &pA0->y );
        /* pPAz = 1 */
        EscEccFe_SetOne( &pPA0.z );
    } else {
    	EscEccPt_Assign( &pPA0, pA0 );
    }

    if ( pA1->type == C_PT_AFFINE ) {
		pPA1.type = C_PT_JACOBIAN;
		/* pPAx = pAx */
		EscEccFe_Assign( &pPA1.x, &pA1->x );
		/* pPAy = pAy */
		EscEccFe_Assign( &pPA1.y, &pA1->y );
		/* pPAz = 1 */
		EscEccFe_SetOne( &pPA1.z );
    } else {
        EscEccPt_Assign( &pPA1, pA1 );
    }

    /* set pJ type */
    pJ->type = C_PT_JACOBIAN;
    EscEccPt_SetInfinity( pJ );

    if ( EscEccFe_IsZero( k0 ) ) {
        EscEccPt_JacobianMultiplyBinary( pJ, k1, &pPA1 );
    } else if ( EscEccFe_IsZero( k1 ) ) {
        EscEccPt_JacobianMultiplyBinary( pJ, k0, &pPA0 );
    } else {
        SINT16 i;
        EscEcc_PointT  helper;

        /* precompute Q + G */
        EscEccPt_JacobianAdd( &helper, &pPA0, &pPA1 );

        i = ( (SINT16)EscEcc_MAX_WORDS * (SINT16)EscEcc_WORD_BITS ) - 1;

        /* skip leading zeros */
        while ( !( ( EscEccFe_isBitSet( k0, i ) == TRUE ) || ( EscEccFe_isBitSet( k1, i ) == TRUE ) ) ) {
            i--;
        }

        /* first step */
        if ( EscEccFe_isBitSet( k0, i ) ) {
            if ( EscEccFe_isBitSet( k1, i ) ) {
                /* k0=1 k1=1 */
                EscEccPt_JacobianAdd( pJ, pJ, &helper );
            } else {
                /* k0=1 k1=0 */
                EscEccPt_JacobianAdd( pJ, pJ, &pPA0 );
            }
        } else {
            if ( EscEccFe_isBitSet( k1, i ) ) {
                /* k0=0 k1=1 */
                EscEccPt_JacobianAdd( pJ, pJ, &pPA1 );
            }
        }
        i--;

        while ( i >= 0 ) {
            /* *2 */
            EscEccPt_JacobianDouble( pJ, pJ );

            if ( EscEccFe_isBitSet( k0, i ) ) {
                if ( EscEccFe_isBitSet( k1, i ) ) {
                    /* k0=1 k1=1 -> add Q+G*/
                    EscEccPt_JacobianAdd( pJ, pJ, &helper );
                } else {
                    /* k0=1 k1=0 */
                    EscEccPt_JacobianAdd( pJ, pJ, &pPA0 );
                }
            } else {
                if ( EscEccFe_isBitSet( k1, i ) ) {
                    /* k0=0 k1=1 */
                    EscEccPt_JacobianAdd( pJ, pJ, &pPA1 );
                }
            }
            i--;
        }
    }
}


/*******************************************
 * generates public key out of private key *
 *******************************************/
BOOL
EscEcc_PublicKeyGeneration(
    EscEcc_PublicKeyT* pQ,
    /* result: public key */
    const UINT8 d[] )
{
    /* private key        */
    BOOL hasFailed = FALSE;

    /* declare buffers */
    EscEcc_FieldElementT d_ecc_field_element;
    EscEcc_PointT pQ_point;

    if ( ( pQ == 0 ) || ( d == 0 ) ) {
        hasFailed = TRUE;
    } else {
        /* allocate field element for d */
        EscEccFe_FromUint8( d, &d_ecc_field_element );

        /* calculate public key pjQ = d × G mod p */
        EscEccPt_JacobianMultiplyBinary( &pQ_point, &d_ecc_field_element, &EscEcc_curve32.base_point_G );

        /* convert jacobian pQ into affine pQ mod p */
        EscEccPt_ToAffine( &pQ_point, &pQ_point );

        /* convert to UINT8 */
        EscEccFe_ToUint8( &pQ_point.x, pQ->x );
        EscEccFe_ToUint8( &pQ_point.y, pQ->y );

    }

    return hasFailed;
}



/**
Assigns pQ to pQ_point and checks for validity
*/
static BOOL
EscEcc_PartialPubKeyValidation(
    EscEcc_PointT* pQ_point,
    const EscEcc_PublicKeyT* pQ )
{
    BOOL hasFailed = TRUE;
    EscEcc_FieldElementT T1;
    EscEcc_FieldElementT T2;
    SINT8 comp;

    if ( pQ != 0U ) {
        pQ_point->type = C_PT_AFFINE;

        /* convert to EscEcc_FieldElementT */
        EscEccFe_FromUint8( pQ->x, &pQ_point->x );
        EscEccFe_FromUint8( pQ->y, &pQ_point->y );

        /* A.) check Q != (0) */
        if ( EscEccPt_IsInfinity( pQ_point ) == FALSE ) {
            /* B.) check that Q(x,y) consists of properly represented elements of GF(p) */
            /* 0 < pQ->x < p */
            comp = EscEccFe_AbsoluteCompare( &pQ_point->x, &EscEcc_curve32.ecc_field_params.prime_p );

            if ( comp == -1 ) {
                /* 0 < pQ->y < p */
                comp = EscEccFe_AbsoluteCompare( &pQ_point->y, &EscEcc_curve32.ecc_field_params.prime_p );
                if ( comp == -1 ) {
                    hasFailed = FALSE;
                }
            }
        }

        if ( hasFailed == FALSE ) {
            /* C.) check Q on curve cGP: y² = x³ + a × x + b */

            /* 01.) T1 = x² mod p */
            EscEccFe_ModularMultiply( &T1, &pQ_point->x, &pQ_point->x, &EscEcc_curve32.ecc_field_params );

            /* 02.) T1 = x³ mod p */
            EscEccFe_ModularMultiply( &T1, &T1, &pQ_point->x, &EscEcc_curve32.ecc_field_params );

            /* 03.) T2 = a × x mod p */
            EscEccFe_ModularMultiply( &T2, &EscEcc_curve32.coefficient_a, &pQ_point->x, &EscEcc_curve32.ecc_field_params );

            /* 04.) T1 = T1 + T2 <=> x³ + a × x mod p */
            EscEccFe_ModularAdd( &T1, &T1, &T2, &EscEcc_curve32.ecc_field_params );

            /* 05.) T1 = T1 + b <=> x³ + a × x + b mod p */
            EscEccFe_ModularAdd( &T1, &T1, &EscEcc_curve32.coefficient_b, &EscEcc_curve32.ecc_field_params );

            /* 06.) T2 = y² mod p */
            EscEccFe_ModularMultiply( &T2, &pQ_point->y, &pQ_point->y, &EscEcc_curve32.ecc_field_params );

            /* 07.) T1 =? T2 <=> y² =? x³ + a × x + b mod p */
            comp = EscEccFe_AbsoluteCompare( &T1, &T2 );
            if ( comp != 0 ) {
                hasFailed = TRUE;
            }
        }
    }

    return hasFailed;
}

/******************************************
 * perform embedded public key validation *
 ******************************************/
BOOL
EscEcc_EmbeddedPKV(
    const EscEcc_PublicKeyT* pQ )
{
    /* declarations */
    BOOL hasFailed;

    /* declare buffers */
    EscEcc_PointT pQ_point;

    /* perform validation */
    hasFailed = EscEcc_PartialPubKeyValidation( &pQ_point, pQ );

    return hasFailed;
}

/***********************
 * validate public key *
 ***********************/
BOOL
EscEcc_PublicKeyValidation(
    const EscEcc_PublicKeyT* pQ )
{
    /* declarations */
    BOOL hasFailed;

    /* declare buffers */
    EscEcc_PointT pQ_point;
    EscEcc_PointT pX;

    /* perform embedded public key validation */
    hasFailed = EscEcc_PartialPubKeyValidation( &pQ_point, pQ );

    if ( hasFailed == FALSE ) {
        /* D.) check that n × Q = (0) mod p */

        EscEccPt_JacobianMultiplyBinary( &pX, &EscEcc_curve32.base_point_order_n.prime_p, &pQ_point );
        EscEccPt_ToAffine( &pX, &pX );

        if ( EscEccPt_IsZero( &pX ) == FALSE ) {
            hasFailed = TRUE;
        }
    }

    return hasFailed;
}

#ifndef EscEcc_ECDH_DISABLE

/*****************************************
 * compute shared secret z based on ECDH *
 *****************************************/
BOOL
EscEcc_ComputeSharedSecret(
    UINT8 z[],                  /* result: shared secret  */
    const UINT8 dA[],           /* A's private key        */
    const EscEcc_PublicKeyT* pQB )
{
    /* B's public key         */
    BOOL hasFailed = FALSE;

    /* declare buffers */
    EscEcc_FieldElementT dA_ecc_field_element;
    EscEcc_FieldElementT z_ecc_field_element;
    EscEcc_PointT pQB_point;
    EscEcc_PointT pX;

    if ( pQB == 0 ) {
        hasFailed = TRUE;
    } else {
        /* allocate field element for dA */
        EscEccFe_FromUint8( dA, &dA_ecc_field_element );

        /* convert to EscEcc_FieldElementT */
        pQB_point.type = C_PT_AFFINE;
        EscEccFe_FromUint8( pQB->x, &pQB_point.x );
        EscEccFe_FromUint8( pQB->y, &pQB_point.y );

        /* 01.) pX = dA × QB mod p */
        EscEccPt_JacobianMultiplyBinary( &pX, &dA_ecc_field_element, &pQB_point );

        /* 02.) check pX != (0) mod p */
        EscEccPt_ToAffine( &pX, &pX );

        if ( EscEccPt_IsZero( &pX ) ) {
            hasFailed = TRUE;
        } else {
            /* 03.) shared secret value z = pX->x */

            /* allocate field element for dA */
            EscEccFe_Assign( &z_ecc_field_element, &pX.x );
            EscEccFe_ToUint8( &z_ecc_field_element, z );
        }
    }

    return hasFailed;
}

#endif /* EscEcc_ECDH */

#ifndef EscEcc_ECDSA_DISABLE
/*****************************************
 * Verify signature for a hashed message *
 *****************************************/
BOOL
EscEcc_SignatureVerification(
    const UINT8 msg_hash[],             /* hash of signing message */
    const EscEcc_SignatureT* ecc_sig,   /* ECC signature           */
    const EscEcc_PublicKeyT* pQ )       /* public key Q            */
{
    BOOL hasFailed = FALSE;

    /* declare buffers */
    EscEcc_FieldElementT w;
    EscEcc_FieldElementT v;
    EscEcc_FieldElementT u1;
    EscEcc_FieldElementT u2;
    EscEcc_FieldElementT r;
    EscEcc_FieldElementT s;
    EscEcc_FieldElementT msg_hash_ecc_field_element;
    EscEcc_FieldElementLongT x1Longx;

    EscEcc_PointT pQ_point;
    EscEcc_PointT pX1;

#    ifndef EscEcc_USE_DUAL_MULTIPLY_ADD
    EscEcc_PointT pX2;
#    endif

    if ( ( msg_hash == 0 ) || ( ecc_sig == 0 ) || ( pQ == 0 ) ) {
        hasFailed = TRUE;
    }

    if ( hasFailed == FALSE ) {
        pQ_point.type = C_PT_AFFINE;
        EscEccFe_FromUint8( pQ->x, &pQ_point.x );
        EscEccFe_FromUint8( pQ->y, &pQ_point.y );

        EscEccFe_FromUint8( ecc_sig->r_bytes, &r );
        EscEccFe_FromUint8( ecc_sig->s_bytes, &s );
        EscEccFe_FromUint8( msg_hash, &msg_hash_ecc_field_element );

        /* 1.) obtain an authentic copy of A's public key (E,G,n,Q) */

        /* 2.) verify that r and s are integers in the interval [ 1, n - 1 ] */

        /* check r */
        if ( EscEccFe_AbsoluteCompare( &r, &EscEcc_curve32.base_point_order_n.prime_p ) != -1 ) {       /* r >= n */
            hasFailed = TRUE;
        }
    }

    if ( hasFailed == FALSE ) {
        /* check s */
        /*lint -save -e645 s is initialized */
        if ( EscEccFe_AbsoluteCompare( &s, &EscEcc_curve32.base_point_order_n.prime_p ) != -1 ) {       /* s >= n */
            hasFailed = TRUE;
        }
        /*lint -restore */
    }

    if ( hasFailed == FALSE ) {
        /* 3.) compute w = s^(-1) mod n */
        EscEccFe_ModularInvert( &w, &s, &EscEcc_curve32.base_point_order_n );

        /* 4.a) compute u1 = h(m) × w mod n */
        /*lint -save -e645 msg_hash_ecc_field_element is initialized */
        EscEccFe_ModularMultiply( &u1, &msg_hash_ecc_field_element, &w, &EscEcc_curve32.base_point_order_n );
        /*lint -restore */

        /* 4.b) compute u2 = r × w mod n */
        /*lint -save -e645 r is initialized */
        EscEccFe_ModularMultiply( &u2, &r, &w, &EscEcc_curve32.base_point_order_n );
        /*lint -restore */

        /* 5.a) compute u1 × G + u2 × Q = ( x0, y0 ) */

#    ifdef EscEcc_USE_DUAL_MULTIPLY_ADD

        /*lint -save -e645 pQ_point is initialized */
        EscEccPt_JacDualMulAddBin( &pX1, &u1, &EscEcc_curve32.base_point_G, &u2, &pQ_point );
        /*lint -restore */

#    else

        EscEccPt_JacobianMultiplyBinary( &pX1, &u1, &EscEcc_curve32.base_point_G );

        /*lint -save -e645 pQ_point is initialized */
        EscEccPt_JacobianMultiplyBinary( &pX2, &u2, &pQ_point );
        /*lint -restore */

        EscEccPt_JacobianAdd( &pX1, &pX1, &pX2 );

#    endif

        EscEccPt_ToAffine( &pX1, &pX1 );

        /* 5.b) compute v = x0 mod n */
        /* we need a long version of x1 */
        EscEccFe_ToLongElement( &x1Longx, &pX1.x );
        EscEccFe_ReduceBarrett( &v, &x1Longx, &EscEcc_curve32.base_point_order_n );

        /* 6.) accept signature if and only if v = r */
        if ( EscEccFe_AbsoluteCompare( &v, &r ) != 0 ) {
            hasFailed = TRUE;
        }
    }

    return hasFailed;
}

/***********************************************
 * generate the signature for a hashed message *
 ***********************************************/
BOOL
EscEcc_SignatureGeneration(
    EscEcc_SignatureT* ecc_sig,     /* result: ECC signature   */
    const UINT8 msg_hash[],     /* hash of signing message */
    const UINT8 d[],            /* app. private key        */
    const UINT8 k[] )
{
    /* unpredictable integer   */
    BOOL hasFailed = FALSE;

    /* declare buffers */
    EscEcc_PointT pX;
    EscEcc_FieldElementT fe_k_inv;
    EscEcc_FieldElementT r;
    EscEcc_FieldElementT s;
    EscEcc_FieldElementT d_ecc_field_element;
    EscEcc_FieldElementT k_ecc_field_element;
    EscEcc_FieldElementT msg_hash_ecc_field_element;
    EscEcc_FieldElementLongT x1Longx;

    /* parameter check */
    if ( ( ecc_sig == 0 ) || ( msg_hash == 0 ) || ( d == 0 ) || ( k == 0 ) ) {
        hasFailed = TRUE;
    }

    if ( hasFailed == FALSE ) {
        /* convert to EscEcc_FieldElementT */
        EscEccFe_FromUint8( msg_hash, &msg_hash_ecc_field_element );
        EscEccFe_FromUint8( d, &d_ecc_field_element );
        EscEccFe_FromUint8( k, &k_ecc_field_element );

        /* 1.) select a statistically unique and unpredictable integer k in the interval [1, n ­ 1] */

        /* 2.) compute k × G = ( x1 , y1 )  */

        /* pX = k × G mod p   */
        EscEccPt_JacobianMultiplyBinary( &pX, &k_ecc_field_element, &EscEcc_curve32.base_point_G );
        EscEccPt_ToAffine( &pX, &pX );

        /* r = pX.x1 mod n */
        EscEccFe_ToLongElement( &x1Longx, &pX.x );
        EscEccFe_ReduceBarrett( &r, &x1Longx, &EscEcc_curve32.base_point_order_n );

        /* if r = 0, then go to step 1 (because s = k^(­l) × { h(m) + d × r} mod n does not involve the private key d! */
        if ( EscEccFe_IsZero( &r ) ) {
            hasFailed = TRUE;
        }
    }

    if ( hasFailed == FALSE ) {
        /* 3.) compute k^(­1) mod n */
        /* fe_k = k^(-1) mod n */
        /*lint -save -e645 k_ecc_field_element, d_ecc_field_element, r, s, msg_hash_ecc_field_element are initialized */
        EscEccFe_ModularInvert( &fe_k_inv, &k_ecc_field_element, &EscEcc_curve32.base_point_order_n );

        /* 4.) compute s = k^(­l) × { h(m) + d × r} mod n (where h is the Secure Hash Algorithm SHA­l) */

        /* s = d × r mod n */
        EscEccFe_ModularMultiply( &s, &d_ecc_field_element, &r, &EscEcc_curve32.base_point_order_n );

        /* s = h(m) + s mod n */
        EscEccFe_ModularAdd( &s, &msg_hash_ecc_field_element, &s, &EscEcc_curve32.base_point_order_n );

        /* s = k^(­l) × s mod n  */
        EscEccFe_ModularMultiply( &s, &fe_k_inv, &s, &EscEcc_curve32.base_point_order_n );

        /* 5.) if s = 0, then go to step 1 (if s = 0, then s^(­1) mod n does not exist, but s^(­1) is required in step 2 of signature verification)  */
        if ( EscEccFe_IsZero( &s ) ) {
            hasFailed = TRUE;
        }
        /*lint -restore */
    }

    if ( hasFailed == FALSE ) {
        /* 6.) signature for the message m is the pair of integers (r, s)  */
        /*lint -save -e645 r, s are initialized */
        EscEccFe_ToUint8( &r, ecc_sig->r_bytes );
        EscEccFe_ToUint8( &s, ecc_sig->s_bytes );
        /*lint -restore */
    }

    return hasFailed;
}

#endif /* EscEcc_ECDSA */

/***************************************************************************
 * 5. END                                                                  *
 ***************************************************************************/
