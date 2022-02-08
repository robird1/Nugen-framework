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
   \file        sha_2.c

   \brief       SHA-224 and SHA-256 implementation also known as SHA-2.

   \see      FIPS-180-2 at
   http://csrc.nist.gov/publications/fips/fips180-2/fips180-2withchangenotice.pdf

   $Rev: 19842 $
 */
/***************************************************************************/

/***************************************************************************
* 1. INCLUDES                                                              *
****************************************************************************/

#include "sha_256.h"

/***************************************************************************
 * 2. DEFINES                                                              *
 ***************************************************************************/

/** Length of schedule in words. */
#define EscSha2MSched_NUM_WORDS 16U

/** Circular right rotation, as defined in function ROTR of section 3.2 point 4. */
#define EscSha256_ROTR( x, n ) ( ( ( x ) >> ( n ) ) | ( ( x ) << ( 32U - ( n ) ) ) )

/** Logical function Ch of sec. 4.1.2 */
#define EscSha256_FuncCh( x, y, z ) ( ( z ) ^ ( ( x ) & ( ( y ) ^ ( z ) ) ) )

/** Logical function Maj of sec. 4.1.2 */
#define EscSha256_FuncMaj( x, y, z ) ( ( ( x ) & ( y ) ) | ( ( z ) & ( ( x ) ^ ( y ) ) ) )

/***************************************************************************
 * 3. DEFINITIONS                                                          *
 ***************************************************************************/
/** Message schedule context according to sec. 6.2.2 step 1 */
typedef struct {
    /**
    W_t. The array keeps the last 16 Wt.
    In the beginning w[0] = W_0, w[1] = W_1, etc.
    For t= 16, w[0] = W_16; t=17, w[1] = W-17, etc.
    */
    UINT32 w[ EscSha2MSched_NUM_WORDS ];
    /* Current t. */
    UINT8 t;
} EscSha2MSched_ContextT;

static void
EscSha2MSched_Init(
    EscSha2MSched_ContextT* sched,
    const UINT8 block[] );

static UINT32
EscSha2MSched_NextW(
    EscSha2MSched_ContextT* sched );

static void
EscSha256_UpdateHash(
    EscSha256_ContextT* ctx );

static void
EscSha256_UpdateHashFast(
    EscSha256_ContextT* ctx,
    const UINT8 block[] );

static void
EscSha256_ApplyPadding(
    EscSha256_ContextT* ctx );

static UINT32
EscSha256_FuncSIGMA_0_256(
    UINT32 x );

static UINT32
EscSha256_FuncSIGMA_1_256(
    UINT32 x );

static UINT32
EscSha256_FuncSigma_0_256(
    UINT32 x );

static UINT32
EscSha256_FuncSigma_1_256(
    UINT32 x );

/***************************************************************************
 * 4. CONSTANTS                                                            *
 ***************************************************************************/

/* Constants from section 4.2.2 */
#define EscSha256_K                                                                                             \
    {                                                                                                           \
        0x428a2f98U, 0x71374491U, 0xb5c0fbcfU, 0xe9b5dba5U, 0x3956c25bU, 0x59f111f1U, 0x923f82a4U, 0xab1c5ed5U, \
        0xd807aa98U, 0x12835b01U, 0x243185beU, 0x550c7dc3U, 0x72be5d74U, 0x80deb1feU, 0x9bdc06a7U, 0xc19bf174U, \
        0xe49b69c1U, 0xefbe4786U, 0x0fc19dc6U, 0x240ca1ccU, 0x2de92c6fU, 0x4a7484aaU, 0x5cb0a9dcU, 0x76f988daU, \
        0x983e5152U, 0xa831c66dU, 0xb00327c8U, 0xbf597fc7U, 0xc6e00bf3U, 0xd5a79147U, 0x06ca6351U, 0x14292967U, \
        0x27b70a85U, 0x2e1b2138U, 0x4d2c6dfcU, 0x53380d13U, 0x650a7354U, 0x766a0abbU, 0x81c2c92eU, 0x92722c85U, \
        0xa2bfe8a1U, 0xa81a664bU, 0xc24b8b70U, 0xc76c51a3U, 0xd192e819U, 0xd6990624U, 0xf40e3585U, 0x106aa070U, \
        0x19a4c116U, 0x1e376c08U, 0x2748774cU, 0x34b0bcb5U, 0x391c0cb3U, 0x4ed8aa4aU, 0x5b9cca4fU, 0x682e6ff3U, \
        0x748f82eeU, 0x78a5636fU, 0x84c87814U, 0x8cc70208U, 0x90befffaU, 0xa4506cebU, 0xbef9a3f7U, 0xc67178f2U  \
    }

/***************************************************************************
 * 5. IMPLEMENTATION OF FUNCTIONS                                          *
 ***************************************************************************/

/** Logical function capital sigma from 0 to 256 of sec. 4.1.2 */
static UINT32
EscSha256_FuncSIGMA_0_256(
    UINT32 x )
{
    UINT32 ret;

    ret = EscSha256_ROTR( x, 2U );
    ret ^= EscSha256_ROTR( x, 13U );
    ret ^= EscSha256_ROTR( x, 22U );

    return ret;
}

/** Logical function capital sigma from 1 to 256 of sec. 4.1.2 */
static UINT32
EscSha256_FuncSIGMA_1_256(
    UINT32 x )
{
    UINT32 ret;

    ret = EscSha256_ROTR( x, 6U );
    ret ^= EscSha256_ROTR( x, 11U );
    ret ^= EscSha256_ROTR( x, 25U );

    return ret;
}

/** Logical function small sigma from 0 to 256 of sec. 4.1.2 */
static UINT32
EscSha256_FuncSigma_0_256(
    UINT32 x )
{
    UINT32 ret;

    ret = EscSha256_ROTR( x, 7U );
    ret ^= EscSha256_ROTR( x, 18U );
    ret ^= ( x >> 3 );

    return ret;
}

/** Logical function small sigma from 1 to 256 of sec. 4.1.2 */
static UINT32
EscSha256_FuncSigma_1_256(
    UINT32 x )
{
    UINT32 ret;

    ret = EscSha256_ROTR( x, 17U );
    ret ^= EscSha256_ROTR( x, 19U );
    ret ^= ( x >> 10 );

    return ret;
}

static void
EscSha2MSched_Init(
    EscSha2MSched_ContextT* sched,
    const UINT8 block[] )
{
    UINT8 i;

    sched->t = 0U;

    /* W_0 .. W_15 is M_0..M-15, see sec. 6.2.2 step 1 */
    for ( i = 0U; i < EscSha2MSched_NUM_WORDS; i++ ) {
        UINT8 offset;

        offset = (UINT8)( 4U * i );
        sched->w[ i ] = ( ( (UINT32)block[ offset ] ) << 24 ) |
            ( ( (UINT32)block[ offset + 1U ] ) << 16 ) |
            ( ( (UINT32)block[ offset + 2U ] ) << 8 ) |
            ( (UINT32)block[ offset + 3U ] );
    }
}

static UINT32
EscSha2MSched_NextW(
    EscSha2MSched_ContextT* sched )
{
    UINT32 ret;
    UINT8 curT;

    /* W_t 6.2.2 step 1 */

    curT = sched->t;

    if ( curT < 16U ) {
        ret = sched->w[ curT ];
    } else {                    /* sched->t >= 16U */
        /* we calculate the indexes. The number -x is congruent to +(16-x) in |F_16 */
        UINT8 iw_2 = ( curT + ( 16U - 2U ) ) & 0x0fU;     /* Index of W_t-2 */
        UINT8 iw_7 = ( curT + ( 16U - 7U ) ) & 0x0fU;     /* Index of W_t-7 */
        UINT8 iw_15 = ( curT + ( 16U - 15U ) ) & 0x0fU;     /* Index of W_t-15 */
        UINT8 iw_16 = curT & 0x0fU; /* Index of W_t-16, and also t%16 */
        UINT32* W;

        W = sched->w;

        /*lint -save -esym(960,17.4) W is an array*/
        ret = EscSha256_FuncSigma_1_256( W[ iw_2 ] ) + W[ iw_7 ] + EscSha256_FuncSigma_0_256( W[ iw_15 ] ) + W[ iw_16 ];

        W[ iw_16 ] = ret;         /* the next W_t-1 */
        /*lint -restore W is an array*/
    }

    sched->t++;                 /* point to next t */

    return ret;
}

/** Padding according to section 5.1.1 */
static void
EscSha256_ApplyPadding(
    EscSha256_ContextT* ctx )
{
    const UINT8 PAD_LENGTH_SIZE = 8U;   /* Size of the length in the padding in byte. (We keep the first 3 byte always zero) */
    UINT32 lengthBits;

    Esc_ASSERT( ctx->blockLen < EscSha256_BLOCK_BYTES );

    /* Append a 0x10 */
    ctx->block[ ctx->blockLen ] = 0x80U;
    ctx->blockLen++;
    if ( ctx->blockLen == EscSha256_BLOCK_BYTES ) {
        EscSha256_UpdateHash( ctx );
    }

    /* Check if there is enough space for the length. If not, fill the
       current block with zeros and hash it */
    if ( ctx->blockLen > ( EscSha256_BLOCK_BYTES - PAD_LENGTH_SIZE ) ) {
        while ( ctx->blockLen < EscSha256_BLOCK_BYTES ) {
            ctx->block[ ctx->blockLen ] = 0U;
            ctx->blockLen++;
        }

        EscSha256_UpdateHash( ctx );
    }

    /* Fill the block with zeros, except the last bytes for the length */
    while ( ctx->blockLen < ( EscSha256_BLOCK_BYTES - PAD_LENGTH_SIZE ) ) {
        ctx->block[ ctx->blockLen ] = 0U;
        ctx->blockLen++;
    }

    /* Append the length in bits */
    lengthBits = ctx->totalLen;
    /* The length in the format 0 | 0 | 0 | b31..b29 | b28..b21 | b20..b13 | b12..b5 | b4..b0 000 */
    /* b4..b0 */
    ctx->block[ EscSha256_BLOCK_BYTES - 1U ] = (UINT8)( ( (UINT8)( lengthBits & 0x1fU ) ) << 3 );
    lengthBits >>= 5;

    /* b12..b5 */
    ctx->block[ ( EscSha256_BLOCK_BYTES - 1U ) - 1U ] = (UINT8)( lengthBits & 0xffU );
    lengthBits >>= 8;

    /* b20..b13 */
    ctx->block[ ( EscSha256_BLOCK_BYTES - 1U ) - 2U ] = (UINT8)( lengthBits & 0xffU );
    lengthBits >>= 8;

    /* b28..b21 */
    ctx->block[ ( EscSha256_BLOCK_BYTES - 1U ) - 3U ] = (UINT8)( lengthBits & 0xffU );
    lengthBits >>= 8;

    /* b31..b29 */
    ctx->block[ ( EscSha256_BLOCK_BYTES - 1U ) - 4U ] = (UINT8)( lengthBits & 0xffU );
    ctx->block[ ( EscSha256_BLOCK_BYTES - 1U ) - 5U ] = 0U;
    ctx->block[ ( EscSha256_BLOCK_BYTES - 1U ) - 6U ] = 0U;
    ctx->block[ ( EscSha256_BLOCK_BYTES - 1U ) - 7U ] = 0U;

    ctx->blockLen += PAD_LENGTH_SIZE;

    Esc_ASSERT( ctx->blockLen == EscSha256_BLOCK_BYTES );
}

/**
Fast hashing of a single hash block.  The block length (not ctx->blockLen)
must be EscSha256_BLOCK_BYTES;
Does the inner loop of section 6.2.2.
ctx->blocklLen is not reset in this function.
*/
static void
EscSha256_UpdateHashFast(
    EscSha256_ContextT* ctx,
    const UINT8 block[] )
{
    static const UINT32 K[] = EscSha256_K;  /* Constants from section 4.2.2 */
    const UINT8* msg;
    UINT32* H;
    EscSha2MSched_ContextT sched;
    UINT8 t;
    UINT32 a, b, c, d, e, f, g, h;
    UINT32 T1, T2;

    msg = block;
    H = ctx->hash;

    /* Step 1 */
    EscSha2MSched_Init( &sched, msg );

    /* Step 2 */
    /*lint -save -esym(960,17.4) H is an array*/
    a = H[ 0 ];
    b = H[ 1 ];
    c = H[ 2 ];
    d = H[ 3 ];
    e = H[ 4 ];
    f = H[ 5 ];
    g = H[ 6 ];
    h = H[ 7 ];
    /*lint -restore H is an array*/

    /* Step 3 */
    for ( t = 0U; t <= 63U; t++ ) {
        T1 = h + EscSha256_FuncSIGMA_1_256( e ) + EscSha256_FuncCh( e, f, g ) + K[ t ] + EscSha2MSched_NextW( &sched );  /* W_t */
        T2 = EscSha256_FuncSIGMA_0_256( a ) + EscSha256_FuncMaj( a, b, c );

        h = g;
        g = f;
        f = e;
        e = d + T1;
        d = c;
        c = b;
        b = a;
        a = T1 + T2;
    }

    /* Step 4 */
    /*lint -save -esym(960,17.4) H is an array*/
    H[ 0 ] += a;
    H[ 1 ] += b;
    H[ 2 ] += c;
    H[ 3 ] += d;
    H[ 4 ] += e;
    H[ 5 ] += f;
    H[ 6 ] += g;
    H[ 7 ] += h;
    /*lint -restore H is an array*/
}

/**
Fast hashing of a single hash block.  The ctx->blockLen must be EscSha256_BLOCK_BYTES;
Does the inner loop of section 6.2.2.
*/
static void
EscSha256_UpdateHash(
    EscSha256_ContextT* ctx )
{
    Esc_ASSERT( ctx->blockLen == EscSha256_BLOCK_BYTES );

    EscSha256_UpdateHashFast( ctx, ctx->block );

    ctx->blockLen = 0U;
}

/***********************************************************************
 ***********************************************************************
 **** Exported functions
 ***********************************************************************
 ***********************************************************************/
BOOL
EscSha256_Init(
    EscSha256_ContextT* ctx )
{
    BOOL hasFailed = TRUE;
    UINT8 i;
    const UINT8 HASH_SIZE = 8U;
    static const UINT32 INITIAL_HASH[ 8 ] = {
#ifdef EscSha256_IS_SHA224
        0xc1059ed8U, 0x367cd507U, 0x3070dd17U, 0xf70e5939U, 0xffc00b31U, 0x68581511U, 0x64f98fa7U, 0xbefa4fa4U
#else
        0x6a09e667U, 0xbb67ae85U, 0x3c6ef372U, 0xa54ff53aU, 0x510e527fU, 0x9b05688cU, 0x1f83d9abU, 0x5be0cd19U
#endif
    };

    if ( ctx != 0 ) {
        /* Set initial hash value Sec. 5.3.2 */
        for ( i = 0U; i < HASH_SIZE; i++ ) {
            ctx->hash[ i ] = INITIAL_HASH[ i ];
        }

        ctx->blockLen = 0U;
        ctx->totalLen = 0U;

        hasFailed = FALSE;
    }

    return hasFailed;
}

BOOL
EscSha256_Update(
    EscSha256_ContextT* ctx,
    const UINT8 payload[],
    UINT32 dataLen )
{
    BOOL hasFailed = TRUE;

    if ( ( ctx != 0 ) && ( payload != 0 ) ) {
        UINT32 bytesLeft = dataLen;
        UINT32 dataPos = 0U;

        while ( bytesLeft > 0U ) {
            if ( ( ctx->blockLen == 0U ) && ( bytesLeft >= EscSha256_BLOCK_BYTES ) ) {
                UINT32 numFullBlocks = bytesLeft / EscSha256_BLOCK_BYTES;
                /* The number of bytes we process with UpdateHashFast() */
                UINT32 fullBlockBytes = numFullBlocks * EscSha256_BLOCK_BYTES;

                bytesLeft -= fullBlockBytes;
                ctx->totalLen += fullBlockBytes;

                while ( dataPos <= ( dataLen - EscSha256_BLOCK_BYTES ) ) {
                    EscSha256_UpdateHashFast( ctx, &payload[ dataPos ] );
                    dataPos += EscSha256_BLOCK_BYTES;
                }

                /* blockLen remains unchanged */
                Esc_ASSERT( ctx->blockLen == 0U );
            } else {
                UINT8 bytesToFill;
                /* fill block with remaining bytes, blockLen is now smaller than EscSha256_BLOCK_BYTES */
                if ( bytesLeft >= ( (UINT32)EscSha256_BLOCK_BYTES - (UINT32)ctx->blockLen ) ) {
                    bytesToFill = EscSha256_BLOCK_BYTES - ctx->blockLen;
                } else {
                    bytesToFill = (UINT8)bytesLeft;
                }

                ctx->totalLen += bytesToFill;
                bytesLeft -= bytesToFill;
                /* Copy the new bytes to the current block */
                while ( bytesToFill > 0U ) {
                    ctx->block[ ctx->blockLen ] = payload[ dataPos ];

                    ctx->blockLen++;
                    dataPos++;
                    bytesToFill--;
                }

                /* if block is complete - hash */
                if ( ctx->blockLen == EscSha256_BLOCK_BYTES ) {
                    EscSha256_UpdateHash( ctx );
                }
            }
        }

        Esc_ASSERT( ctx->blockLen < EscSha256_BLOCK_BYTES );

        hasFailed = FALSE;
    }

    return hasFailed;
}

BOOL
EscSha256_Finish(
    EscSha256_ContextT* ctx,
    UINT8 digest[] )
{
    BOOL hasFailed = TRUE;
    UINT8 i;

    if ( ( ctx != 0 ) && ( digest != 0 ) ) {
        /* Pad data according to Sec. 5.1.1 */
        EscSha256_ApplyPadding( ctx );
        /* Hash last full block */
        EscSha256_UpdateHash( ctx );

        /* Copy digest back */
        for ( i = 0U; i < ( EscSha256_DIGEST_LEN / 4U ); i++ ) {
            UINT8 offset = (UINT8)( i * 4U );
            UINT32 h = ctx->hash[ i ];

            digest[ offset + 3U ] = (UINT8)( h & 0xffU );
            h >>= 8;
            digest[ offset + 2U ] = (UINT8)( h & 0xffU );
            h >>= 8;
            digest[ offset + 1U ] = (UINT8)( h & 0xffU );
            h >>= 8;
            digest[ offset ] = (UINT8)( h & 0xffU );
        }

        hasFailed = FALSE;
    }

    return hasFailed;
}

BOOL
EscSha256_Calc(
    const UINT8 payload[],
    UINT32 dataLen,
    UINT8 digest[] )
{
    BOOL hasFailed = TRUE;
    EscSha256_ContextT ctx;

    if ( ( payload != 0 ) && ( digest != 0 ) ) {
        hasFailed = EscSha256_Init( &ctx );

        if ( hasFailed == FALSE ) {
            hasFailed = EscSha256_Update( &ctx, payload, dataLen );

            if ( hasFailed == FALSE ) {
                hasFailed = EscSha256_Finish( &ctx, digest );
            }
        }
    }

    return hasFailed;
}

/***************************************************************************
 * 6. END                                                                  *
 ***************************************************************************/
