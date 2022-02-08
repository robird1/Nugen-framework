/***************************************************************************
 *                                                                         *
 * Copyright                                                               *
 *     escrypt GmbH, Bochum, Germany                                       *
 *     Lise-Meitner-Allee 4                                                *
 *     D-44801 Bochum, Germany                                             *
 *                                                                         *
 *     http://www.escrypt.com                                              *
 *     info"at"escrypt.com                                                 *
 *                                                                         *
 * All Rights reserved                                                     *
 *                                                                         *
 ***************************************************************************/

/***************************************************************************/
/*!
   \file        hash_drbg.c

   \brief       Pseudo Random Number Generator w/ SHA-*

   \see         NIST publication SP 800-90:
                Recommendation for Random Number Generation Using
                Deterministic Random Bit Generators - HASH_DRBG

 */
/***************************************************************************
$Author: AdamChen $
$Date: 2011-03-03 02:54:03 +0100 (Do, 03. Mrz 2011) $

History:
19-May-2010 CP --- Creation ---
26-Jan-2011 DV Changed sha1_rng to hash_drbg
02-Mar-2011 DV ------------------------ Version 2.7 ------------------------
$ENDOFHISTORY$
****************************************************************************/

/***************************************************************************
* 1. INCLUDES                                                              *
****************************************************************************/

#include "hash_drbg.h"

/***************************************************************************
* 2. DEFINES                                                              *
***************************************************************************/

/* plausibility checks */
#if EscHashDrbg_ENTROPY_LEN < ( EscHashDrbg_SECURITY_STRENGTH / 8U )
#    error "Entropy must be at least security strength!"
#endif

#if ( EscHashDrbg_SECURITY_STRENGTH == 192U ) || ( EscHashDrbg_SECURITY_STRENGTH == 256U )
#    if ( EscHashDrbg_SHA_TYPE == 1 )
#        error "For security strength above 128 you have to use SHA-224 at least!"
#    endif
#else
#    if ( EscHashDrbg_SECURITY_STRENGTH != 128U )
#        error "Security strength may only be one out of 128, 192, and 256!"
#    endif
#endif

#if EscHashDrbg_NONCE_LEN < ( EscHashDrbg_SECURITY_STRENGTH / 16U )
#    error "Nonce must be at least half the security strength!"
#endif

#define EscHashDrbg_ROUNDS ( EscHashDrbg_SEED_LEN / EscHashDrbg_BLOCK_LEN )
#define EscHashDrbg_REMAINDER ( EscHashDrbg_SEED_LEN % EscHashDrbg_BLOCK_LEN )

/***************************************************************************
* 3. DEFINITIONS                                                          *
***************************************************************************/

/* Function for deriving the new seed and the new state out of the input
 * entropy */
static BOOL
EscHashDrbg_DF(
    const UINT8 inputBytes[],
    UINT8 hashInput[],
    const UINT32 inputBytesLen,
    UINT8 output[] );

/* Function to generate the output (random) bits */
#ifdef EscHashDrbg_ADJUSTABLE_OUTPUT_LENGTH
static BOOL
EscHashDrbg_HashGen(
    const UINT8 seed[],
    UINT16 len,
    UINT8 rnd[] );

#else
static BOOL
EscHashDrbg_HashGen(
    const UINT8 seed[],
    UINT8 rnd[] );

#endif

/* Function to generate the new internal PRNG state and reset the reseed counter */
static BOOL
EscHashDrbg_NewState(
    EscHashDrbg_ContextT* ctx );

/***************************************************************************
* 4. CONSTANTS                                                            *
***************************************************************************/

/***************************************************************************
* 5. IMPLEMENTATION OF FUNCTIONS                                          *
***************************************************************************/

static BOOL
EscHashDrbg_DF(
    const UINT8 inputBytes[],
    UINT8 hashInput[],
    const UINT32 inputBytesLen,
    UINT8 output[] )
{
    BOOL hasFailed = FALSE;
    UINT8 j, i = 0U;
    UINT8 hashResult[ EscHashDrbg_BLOCK_LEN ];

    /* Input array for hash function (hashInput):
     * (counter || length in bit of seed and state || inputBytes) */

    /* set counter, counter begins with 1 according to NIST recommendation 10.4 */
    hashInput[ 0 ] = 0x01U;

    /* set length in bit of seed and state (440 / 888) in big endian */
    hashInput[ 1 ] = 0x00U;
    hashInput[ 2 ] = 0x00U;
    hashInput[ 3 ] = (UINT8)( EscHashDrbg_SEED_LEN >> 5 ) & 0xffU;
    hashInput[ 4 ] = (UINT8)( EscHashDrbg_SEED_LEN << 3 ) & 0xffU;

    /* copy inputBytes into the hash input array */
    for ( j = 0U; j < inputBytesLen; j++ ) {
        hashInput[ j + 5U ] = inputBytes[ j ];
    }

    /* fill the output array with the hash result. Output array has the length of
     * seed and state (EscHashDrbg_SEED_LEN), but the SHA output is only EscHashDrbg_BLOCK_LEN bytes. As a
     * result we use the SHA operation several times. */
    while ( ( hasFailed == FALSE ) && ( i < EscHashDrbg_ROUNDS ) ) {
        /* output array has EscHashDrbg_SEED_LEN bytes (length of seed / state),
         * EscHashDrbg_ROUNDS times of full SHA output (EscHashDrbg_BLOCK_LEN bytes) and in
         * the last round the remaining bytes of the SHA output */
#if ( EscHashDrbg_SHA_TYPE == 1 )
        /* calculate SHA1 */
        hasFailed = EscSha1_Calc( hashInput, ( inputBytesLen + 5U ), &output[ ( i * EscHashDrbg_BLOCK_LEN ) ] );
#endif
#if ( EscHashDrbg_SHA_TYPE == 2 )
        /* calculate SHA-224 / SHA-256 */
        hasFailed = EscSha256_Calc( hashInput, ( inputBytesLen + 5U ), &output[ ( i * EscHashDrbg_BLOCK_LEN ) ] );
#endif
#if ( EscHashDrbg_SHA_TYPE == 3 )
        /* calculate SHA-384 / SHA-512 */
        hasFailed = EscSha512_Calc( hashInput, ( inputBytesLen + 5U ), &output[ ( i * EscHashDrbg_BLOCK_LEN ) ] );
#endif
        /* increment loop counter */
        i++;

        /* set next counter, counter begins with 1 according to NIST recommendation 10.4 */
        hashInput[ 0 ] = i + 1U;
    }

#if ( EscHashDrbg_REMAINDER > 0U )

    if ( hasFailed == FALSE ) {
        /* last round, use only the first bytes of the SHA output */
#    if ( EscHashDrbg_SHA_TYPE == 1 )
        /* calculate SHA1 */
        hasFailed = EscSha1_Calc( hashInput, ( inputBytesLen + 5U ), hashResult );
#    endif
#    if ( EscHashDrbg_SHA_TYPE == 2 )
        /* calculate SHA-224 / SHA-256 */
        hasFailed = EscSha256_Calc( hashInput, ( inputBytesLen + 5U ), hashResult );
#    endif
#    if ( EscHashDrbg_SHA_TYPE == 3 )
        /* calculate SHA-384 / SHA-512 */
        hasFailed = EscSha512_Calc( hashInput, ( inputBytesLen + 5U ), hashResult );
#    endif

        /* copy the first bytes of SHA result into output array */
        for ( j = 0U; j < EscHashDrbg_REMAINDER; j++ ) {
            output[ ( i * EscHashDrbg_BLOCK_LEN ) + j ] = hashResult[ j ];
        }
    }
#endif

    return hasFailed;
}

#ifdef EscHashDrbg_ADJUSTABLE_OUTPUT_LENGTH
static BOOL
EscHashDrbg_HashGen(
    const UINT8 seed[],
    UINT16 len,
    UINT8 rnd[] )
{
    BOOL hasFailed = FALSE;
    UINT32 j = 0U;
    UINT16 carry, rounds, i;
    UINT8 hashResult[ EscHashDrbg_BLOCK_LEN ];
    UINT8 data[ EscHashDrbg_SEED_LEN ];

    /* copy seed into temp array */
    for ( i = 0U; i < EscHashDrbg_SEED_LEN; i++ ) {
        data[ i ] = seed[ i ];
    }

    /* Generate hash blocks until requested number of bytes are reached */
    rounds = len / EscHashDrbg_BLOCK_LEN;

    while ( ( hasFailed == FALSE ) && ( j < rounds ) ) {
#    if ( EscHashDrbg_SHA_TYPE == 1 )
        /* calculate SHA1 of data array */
        hasFailed = EscSha1_Calc( data, EscHashDrbg_SEED_LEN, &rnd[ ( j * EscHashDrbg_BLOCK_LEN ) ] );
#    endif
#    if ( EscHashDrbg_SHA_TYPE == 2 )
        /* calculate SHA-224 / SHA-256 of data array */
        hasFailed = EscSha256_Calc( data, EscHashDrbg_SEED_LEN, &rnd[ ( j * EscHashDrbg_BLOCK_LEN ) ] );
#    endif
#    if ( EscHashDrbg_SHA_TYPE == 3 )
        /* calculate SHA-384 / SHA-512 of data array */
        hasFailed = EscSha512_Calc( data, EscHashDrbg_SEED_LEN, &rnd[ ( j * EscHashDrbg_BLOCK_LEN ) ] );
#    endif

        /* Adding 0x01 to data for next hash */
        i = 0U;
        carry = 0x01U;
        do {
            carry += (UINT16)data[ ( EscHashDrbg_SEED_LEN - 1U ) - i ];
            /* copy new value */
            data[ ( EscHashDrbg_SEED_LEN - 1U ) - i ] = (UINT8)( carry & 0xFFU );
            /* copy the upper 8 bit into carry */
            carry >>= 8U;
            i++;
        } while ( ( carry != 0U ) && ( i < EscHashDrbg_SEED_LEN ) );

        j++;
    }

    /* check if a remainder exist -> last round */
    if ( ( hasFailed == FALSE ) && ( ( len % EscHashDrbg_BLOCK_LEN ) != 0U ) ) {
#    if ( EscHashDrbg_SHA_TYPE == 1 )
        /* calculate SHA1 of data array */
        hasFailed = EscSha1_Calc( data, EscHashDrbg_SEED_LEN, hashResult );
#    endif
#    if ( EscHashDrbg_SHA_TYPE == 2 )
        /* calculate SHA-224 / SHA-256 of data array */
        hasFailed = EscSha256_Calc( data, EscHashDrbg_SEED_LEN, hashResult );
#    endif
#    if ( EscHashDrbg_SHA_TYPE == 3 )
        /* calculate SHA-384 / SHA-512 of data array */
        hasFailed = EscSha512_Calc( data, EscHashDrbg_SEED_LEN, hashResult );
#    endif

        if ( hasFailed == FALSE ) {
            /* copy hash to remaining output bytes */
            for ( i = 0U; i < ( len % EscHashDrbg_BLOCK_LEN ); i++ ) {
                rnd[ ( j * EscHashDrbg_BLOCK_LEN ) + i ] = hashResult[ i ];
            }
        }
    }

    return hasFailed;
}

#else
static BOOL
EscHashDrbg_HashGen(
    const UINT8 seed[],
    UINT8 rnd[] )
{
#    if ( EscHashDrbg_SHA_TYPE == 1 )
    /* calculate SHA1 of the seed */
    return EscSha1_Calc( seed, EscHashDrbg_SEED_LEN, rnd );
#    endif
#    if ( EscHashDrbg_SHA_TYPE == 2 )
    /* calculate SHA-224 / SHA-256 of the seed */
    return EscSha256_Calc( seed, EscHashDrbg_SEED_LEN, rnd );
#    endif
#    if ( EscHashDrbg_SHA_TYPE == 3 )
    /* calculate SHA-384 / SHA-512 of the seed */
    return EscSha512_Calc( seed, EscHashDrbg_SEED_LEN, rnd );
#    endif
}

#endif

static BOOL
EscHashDrbg_NewState(
    EscHashDrbg_ContextT* ctx )
{
    BOOL hasFailed;
    UINT8 i;
    UINT8 stateMaterial[ 1U + EscHashDrbg_SEED_LEN ];
    UINT8 hashInput[ EscHashDrbg_SEED_LEN + 6U ];

    /* compute new state */
    stateMaterial[ 0 ] = 0x00U;
    for ( i = 0U; i < EscHashDrbg_SEED_LEN; i++ ) {
        stateMaterial[ i + 1U ] = ctx->seed[ i ];
    }

    /* derive new state */
    hasFailed = EscHashDrbg_DF( stateMaterial, hashInput, ( EscHashDrbg_SEED_LEN + 1U ), ctx->state );

    if ( hasFailed == FALSE ) {
        /* set reseed counter */
        ctx->counter = 1U;
    }

    return hasFailed;
}

BOOL
EscHashDrbg_Init(
    EscHashDrbg_ContextT* ctx,
    const UINT8 entropy[],
    const UINT8 nonce[] )
{
    BOOL hasFailed = FALSE;
    UINT32 i;
    UINT8 seedMaterial[ EscHashDrbg_ENTROPY_LEN + EscHashDrbg_NONCE_LEN ];
    UINT8 hashInput[ ( EscHashDrbg_ENTROPY_LEN + EscHashDrbg_NONCE_LEN ) + 5U ];

    if ( ( ctx == 0 ) || ( entropy == 0 ) || ( nonce == 0 ) ) {
        hasFailed = TRUE;
    } else {
        /* merge entropy array and nonce into one array */
        for ( i = 0U; i < EscHashDrbg_ENTROPY_LEN; i++ ) {
            seedMaterial[ i ] = entropy[ i ];
        }
        for ( i = 0U; i < EscHashDrbg_NONCE_LEN; i++ ) {
            seedMaterial[ i + EscHashDrbg_ENTROPY_LEN ] = nonce[ i ];
        }

        /* compute new seed */
        hasFailed = EscHashDrbg_DF( seedMaterial, hashInput, ( EscHashDrbg_ENTROPY_LEN + EscHashDrbg_NONCE_LEN ), ctx->seed );

        if ( hasFailed == FALSE ) {
            /* compute new internal state and reset reseed counter */
            hasFailed = EscHashDrbg_NewState( ctx );
        }
    }

    return hasFailed;
}

BOOL
EscHashDrbg_Reseed(
    EscHashDrbg_ContextT* ctx,
    const UINT8 entropy[] )
{
    BOOL hasFailed = FALSE;
    UINT32 i;
    UINT8 seedMaterial[ 1U + EscHashDrbg_SEED_LEN + EscHashDrbg_ENTROPY_LEN ];
    UINT8 hashInput[ EscHashDrbg_SEED_LEN + EscHashDrbg_ENTROPY_LEN + 6U ];

    if ( ( ctx == 0 ) || ( entropy == 0 ) ) {
        hasFailed = TRUE;
    } else {
        /* merge Hash_df input into one array */
        seedMaterial[ 0 ] = 0x01U;
        for ( i = 0U; i < EscHashDrbg_SEED_LEN; i++ ) {
            seedMaterial[ i + 1U ] = ctx->seed[ i ];
        }
        for ( i = 0U; i < EscHashDrbg_ENTROPY_LEN; i++ ) {
            seedMaterial[ EscHashDrbg_SEED_LEN + 1U + i ] = entropy[ i ];
        }

        /* compute new seed */
        hasFailed = EscHashDrbg_DF( seedMaterial, hashInput, ( 1U + EscHashDrbg_SEED_LEN + EscHashDrbg_ENTROPY_LEN ), ctx->seed );

        if ( hasFailed == FALSE ) {
            /* compute new internal state and reset reseed counter */
            hasFailed = EscHashDrbg_NewState( ctx );
        }
    }

    return hasFailed;
}

#ifdef EscHashDrbg_ADJUSTABLE_OUTPUT_LENGTH
BOOL
EscHashDrbg_GetRandom(
    EscHashDrbg_ContextT* ctx,
    UINT8 rnd[],
    UINT16 len )
#else
BOOL
EscHashDrbg_GetRandom(
    EscHashDrbg_ContextT* ctx,
    UINT8 rnd[] )
#endif
{
    BOOL hasFailed = FALSE;
    UINT32 i;
    UINT16 carry = 0U;
    UINT8 hash[ EscHashDrbg_SEED_LEN + 1U ];
    UINT8 hashResult[ EscHashDrbg_BLOCK_LEN ];

    if ( ( ctx == 0 ) || ( rnd == 0 ) ||
#ifdef EscHashDrbg_ADJUSTABLE_OUTPUT_LENGTH
         ( len == 0U ) || ( ctx->counter > EscHashDrbg_RESEED_INTERVAL )
#else
         ( ctx->counter > EscHashDrbg_RESEED_INTERVAL )
#endif
          )
    {
        hasFailed = TRUE;
    } else {
        /* get random bits */
#ifdef EscHashDrbg_ADJUSTABLE_OUTPUT_LENGTH
        hasFailed = EscHashDrbg_HashGen( ctx->seed, len, rnd );
#else
        hasFailed = EscHashDrbg_HashGen( ctx->seed, rnd );
#endif

        if ( hasFailed == FALSE ) {
            /* construct new hash input */
            hash[ 0 ] = 0x03U;
            for ( i = 0U; i < EscHashDrbg_SEED_LEN; i++ ) {
                hash[ i + 1U ] = ctx->seed[ i ];
            }

#if ( EscHashDrbg_SHA_TYPE == 1 )
            /* compute new temp array H (see NIST recommendation 10.1.1.4) with SHA1 */
            hasFailed = EscSha1_Calc( hash, ( 1U + EscHashDrbg_SEED_LEN ), hashResult );
#endif
#if ( EscHashDrbg_SHA_TYPE == 2 )
            /* compute new temp array H (see NIST recommendation 10.1.1.4) with SHA-224/SHA-256 */
            hasFailed = EscSha256_Calc( hash, ( 1U + EscHashDrbg_SEED_LEN ), hashResult );
#endif
#if ( EscHashDrbg_SHA_TYPE == 3 )
            /* compute new temp array H (see NIST recommendation 10.1.1.4) with SHA-384/SHA-512 */
            hasFailed = EscSha512_Calc( hash, ( 1U + EscHashDrbg_SEED_LEN ), hashResult );
#endif

            if ( hasFailed == FALSE ) {
                /* compute new seed */
                /* addition of the first 4 bytes with the counter value */
                for ( i = 0U; i < 4U; i++ ) {
                    carry += (UINT16)( ctx->seed[ ( EscHashDrbg_SEED_LEN - 1U ) - i ] ) + (UINT16)( ctx->state[ ( EscHashDrbg_SEED_LEN - 1U ) - i ] ) + (UINT16)( hashResult[ ( EscHashDrbg_BLOCK_LEN - 1U ) - i ] ) + (UINT16)( ( ctx->counter >> ( i * 8U ) ) & 0xFFU );

                    /* write new seed element */
                    ctx->seed[ ( EscHashDrbg_SEED_LEN - 1U ) - i ] = (UINT8)( carry & 0xFFU );

                    /* copy the upper 8 bit into the carry */
                    carry >>= 8U;
                }

                /* addition of the first 20 bytes with temp array H */
                for (; i < EscHashDrbg_BLOCK_LEN; i++ ) {
                    carry += (UINT16)( ctx->seed[ ( EscHashDrbg_SEED_LEN - 1U ) - i ] ) + (UINT16)( ctx->state[ ( EscHashDrbg_SEED_LEN - 1U ) - i ] ) + (UINT16)( hashResult[ ( EscHashDrbg_BLOCK_LEN - 1U ) - i ] );

                    /* write new seed element */
                    ctx->seed[ ( EscHashDrbg_SEED_LEN - 1U ) - i ] = (UINT8)( carry & 0xFFU );

                    /* copy the upper 8 bit into the carry */
                    carry >>= 8U;
                }

                /* addition of the remaining 35 seed bytes with the state */
                for (; i < EscHashDrbg_SEED_LEN; i++ ) {
                    carry += (UINT16)( ctx->seed[ ( EscHashDrbg_SEED_LEN - 1U ) - i ] ) + (UINT16)( ctx->state[ ( EscHashDrbg_SEED_LEN - 1U ) - i ] );

                    /* write new seed element */
                    ctx->seed[ ( EscHashDrbg_SEED_LEN - 1U ) - i ] = (UINT8)( carry & 0xFFU );

                    /* copy the upper 8 bit into the carry */
                    carry >>= 8U;
                }
                /* discard the last carry */

                /* update reseed counter */
                ctx->counter++;
            }
        }
    }

    return hasFailed;
}

/***************************************************************************
* 6. END                                                                  *
***************************************************************************/
