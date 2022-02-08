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
   \file        ecc_curves32.c

   \brief       Elliptic curve definitions with 32-bit words

   $Rev: 19842 $
 */
/***************************************************************************/

/***************************************************************************
 * 1. INCLUDES                                                             *
 ***************************************************************************/

#include "ecc_curves32.h"

/***************************************************************************
 * 2. DEFINES                                                              *
 ***************************************************************************/

/* CAUTION! - Little-Endian representation: i.e., all arrays are like  */
/* (a[0], a[1], .. , a[n]) there a[0] is the lowest and a[n] the highest word */
#if ( EscEcc_CURVE_TYPE == EscEcc_secPr1 )
#   if ( EscEcc_KEY_LENGTH == 160U )
/***************************************************************************
 * 4.1 SECP160R1 (y^2 = x^3 + a·x + b over GF(p) with p = 2^160 - 2^31 - 1)  *
 ***************************************************************************/
/* UINT32 arrays */
#    define EscEcc_PRIME_P            { 0x7fffffffUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_A      { 0x7ffffffcUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_B      { 0xc565fa45UL, 0x81d4d4adUL, 0x65acf89fUL, 0x54bd7a8bUL, 0x1c97befcUL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_X     { 0x13cbfc82UL, 0x68c38bb9UL, 0x46646989UL, 0x8ef57328UL, 0x4a96b568UL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Y     { 0x7ac5fb32UL, 0x04235137UL, 0x59dcc912UL, 0x3168947dUL, 0x23a62855UL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Z     { 0x00000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_order_n { 0xca752257UL, 0xf927aed3UL, 0x0001f4c8UL, 0x00000000UL, 0x00000000UL, 0x00000001UL, 0x00000000UL }
#    define EscEcc_MY_P               { 0x80000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000001UL, 0x00000000UL }
#    define EscEcc_MY_N               { 0x436AB204UL, 0xD3A1AB09UL, 0x358ADDACUL, 0x06D8512CUL, 0xFFFE0B37UL, 0xFFFFFFFFUL, 0xFFFFFFFFUL }
#    define EscEcc_PLEN 5U
#    define EscEcc_NLEN 6U
    /* EscEcc_KEY_LENGTH == 160 */
#   elif ( EscEcc_KEY_LENGTH == 192U )
/***************************************************************************
 * 4.2 SECP192R1 (y^2 = x^3 + a·x + b over GF(p) with p = 2^192 - 2^64 - 1)  *
 ***************************************************************************/
/* UINT32 arrays */
#    define EscEcc_PRIME_P             { 0xffffffffUL, 0xffffffffUL, 0xfffffffeUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_A       { 0xfffffffcUL, 0xffffffffUL, 0xfffffffeUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_B       { 0xc146b9b1UL, 0xfeb8deecUL, 0x72243049UL, 0x0fa7e9abUL, 0xe59c80e7UL, 0x64210519UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_X      { 0x82ff1012UL, 0xf4ff0afdUL, 0x43a18800UL, 0x7cbf20ebUL, 0xb03090f6UL, 0x188da80eUL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Y      { 0x1e794811UL, 0x73f977a1UL, 0x6b24cdd5UL, 0x631011edUL, 0xffc8da78UL, 0x07192b95UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Z      { 0x00000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_order_n  { 0xb4d22831UL, 0x146bc9b1UL, 0x99def836UL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0x00000000UL }
#    define EscEcc_MY_P                { 0x00000001UL, 0x00000000UL, 0x00000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000001UL }
#    define EscEcc_MY_N                { 0x4B2DD7CFUL, 0xEB94364EUL, 0x662107C9UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000001UL }
#    define EscEcc_PLEN 6U
#    define EscEcc_NLEN 6U
    /* EscEcc_KEY_LENGTH == 192 */
#   elif ( EscEcc_KEY_LENGTH == 224U )
/***************************************************************************
 * 4.3 SECP224 (y^2 = x^3 + a·x + b over GF(p) with p = 2   ^224-2^96 + 1) *
 ***************************************************************************/
/* UINT32 arrays */
#    define EscEcc_PRIME_P             { 0x00000001UL, 0x00000000UL, 0x00000000UL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_A       { 0xfffffffeUL, 0xffffffffUL, 0xffffffffUL, 0xfffffffeUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_B       { 0x2355ffb4UL, 0x270b3943UL, 0xd7bfd8baUL, 0x5044b0b7UL, 0xf5413256UL, 0x0c04b3abUL, 0xb4050a85UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_X      { 0x115c1d21UL, 0x343280d6UL, 0x56c21122UL, 0x4a03c1d3UL, 0x321390b9UL, 0x6bb4bf7fUL, 0xb70e0cbdUL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Y      { 0x85007e34UL, 0x44d58199UL, 0x5a074764UL, 0xcd4375a0UL, 0x4c22dfe6UL, 0xb5f723fbUL, 0xbd376388UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Z      { 0x00000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_order_n  { 0x5c5c2a3dUL, 0x13dd2945UL, 0xe0b8f03eUL, 0xffff16a2UL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0x00000000UL }
#    define EscEcc_MY_P                { 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000001UL }
#    define EscEcc_MY_N                { 0xa3a3d5c3UL, 0xec22d6baUL, 0x1f470fc1UL, 0x0000e95dUL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000001UL }
#    define EscEcc_PLEN 7U
#    define EscEcc_NLEN 7U
    /* EscEcc_KEY_LENGTH == 256 */
#   elif ( EscEcc_KEY_LENGTH == 256U )
/***************************************************************************
 * 4.4 SECP256R1 (y^2 = x^3 + a·x + b over GF(p) with p = 2^224(2^32-1) +2^192+2^96 - 1) *
 ***************************************************************************/
/* UINT32 arrays */
#    define EscEcc_PRIME_P             { 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000001UL, 0xffffffffUL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_A       { 0xfffffffcUL, 0xffffffffUL, 0xffffffffUL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000001UL, 0xffffffffUL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_B       { 0x27d2604bUL, 0x3bce3c3eUL, 0xcc53b0f6UL, 0x651d06b0UL, 0x769886bcUL, 0xb3ebbd55UL, 0xaa3a93e7UL, 0x5ac635d8UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_X      { 0xd898c296UL, 0xf4a13945UL, 0x2deb33a0UL, 0x77037d81UL, 0x63a440f2UL, 0xf8bce6e5UL, 0xe12c4247UL, 0x6b17d1f2UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Y      { 0x37bf51f5UL, 0xcbb64068UL, 0x6b315eceUL, 0x2bce3357UL, 0x7c0f9e16UL, 0x8ee7eb4aUL, 0xfe1a7f9bUL, 0x4fe342e2UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Z      { 0x00000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_order_n  { 0xfc632551UL, 0xf3b9cac2UL, 0xa7179e84UL, 0xbce6faadUL, 0xffffffffUL, 0xffffffffUL, 0x00000000UL, 0xffffffffUL, 0x00000000UL }
#    define EscEcc_MY_P                { 0x00000003UL, 0x00000000UL, 0xffffffffUL, 0xfffffffeUL, 0xfffffffeUL, 0xfffffffeUL, 0xffffffffUL, 0x00000000UL, 0x00000001UL }
#    define EscEcc_MY_N                { 0xeedf9bfeUL, 0x012ffd85UL, 0xdf1a6c21UL, 0x43190552UL, 0xffffffffUL, 0xfffffffeUL, 0xffffffffUL, 0x00000000UL, 0x00000001UL }
#    define EscEcc_PLEN 8U
#    define EscEcc_NLEN 8U
    /* EscEcc_KEY_LENGTH == 256 */
#   elif ( EscEcc_KEY_LENGTH == 384U )
/***************************************************************************
 * 4.5 SECP384R1 (y^2 = x^3 + a·x + b over GF(p) with p = p = 2^384 - 2^128 - 2^96 + 2^32 - 1) *
 ***************************************************************************/
/* UINT32 arrays */
#    define EscEcc_PRIME_P             { 0xffffffffUL, 0x00000000UL, 0x00000000UL, 0xffffffffUL, 0xfffffffeUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_A       { 0xfffffffcUL, 0x00000000UL, 0x00000000UL, 0xffffffffUL, 0xfffffffeUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0xffffffffUL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_B       { 0xd3ec2aefUL, 0x2a85c8edUL, 0x8a2ed19dUL, 0xc656398dUL, 0x5013875aUL, 0x0314088fUL, 0xfe814112UL, 0x181d9c6eUL, 0xe3f82d19UL, 0x988e056bUL, 0xe23ee7e4UL, 0xb3312fa7UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_X      { 0x72760AB7UL, 0x3A545E38UL, 0xBF55296CUL, 0x5502F25DUL, 0x82542A38UL, 0x59F741E0UL, 0x8BA79B98UL, 0x6E1D3B62UL, 0xF320AD74UL, 0x8EB1C71EUL, 0xBE8B0537UL, 0xAA87CA22UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Y      { 0x90EA0E5FUL, 0x7A431D7CUL, 0x1D7E819DUL, 0x0A60B1CEUL, 0xB5F0B8C0UL, 0xE9DA3113UL, 0x289A147CUL, 0xF8F41DBDUL, 0x9292DC29UL, 0x5D9E98BFUL, 0x96262C6FUL, 0x3617DE4AUL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Z      { 0x00000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_order_n  { 0xCCC52973UL, 0xECEC196AUL, 0x48B0A77AUL, 0x581A0DB2UL, 0xF4372DDFUL, 0xC7634D81UL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0x00000000UL }
#    define EscEcc_MY_P                { 0x00000001UL, 0xffffffffUL, 0xffffffffUL, 0x00000000UL, 0x00000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000001UL }
#    define EscEcc_MY_N                { 0x333ad68dUL, 0x1313e695UL, 0xb74f5885UL, 0xa7e5f24dUL, 0x0bc8d220UL, 0x389cb27eUL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000001UL }
#    define EscEcc_PLEN 12U
#    define EscEcc_NLEN 12U
    /* EscEcc_KEY_LENGTH == 384 */
#   elif ( EscEcc_KEY_LENGTH == 521U )
/***************************************************************************
 * 4.6 SECP521R1 (y^2 = x^3 + a·x + b over GF(p) with p = p = 2^521 - 1) *
 ***************************************************************************/
/* UINT32 arrays */
#    define EscEcc_PRIME_P             { 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0x000001FFUL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_A       { 0xFFFFFFFCUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0x000001FFUL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_B       { 0x6B503F00UL, 0xEF451FD4UL, 0x3D2C34F1UL, 0x3573DF88UL, 0x3BB1BF07UL, 0x1652C0BDUL, 0xEC7E937BUL, 0x56193951UL, 0x8EF109E1UL, 0xB8B48991UL, 0x99B315F3UL, 0xA2DA725BUL, 0xB68540EEUL, 0x929A21A0UL, 0x8E1C9A1FUL, 0x953EB961UL, 0x00000051UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_X      { 0xc2e5bd66UL, 0xf97e7e31UL, 0x856a429bUL, 0x3348b3c1UL, 0xa2ffa8deUL, 0xfe1dc127UL, 0xefe75928UL, 0xa14b5e77UL, 0x6b4d3dbaUL, 0xf828af60UL, 0x053fb521UL, 0x9c648139UL, 0x2395b442UL, 0x9e3ecb66UL, 0x0404e9cdUL, 0x858e06b7UL, 0x000000c6UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Y      { 0x9fd16650UL, 0x88be9476UL, 0xa272c240UL, 0x353c7086UL, 0x3fad0761UL, 0xc550b901UL, 0x5ef42640UL, 0x97ee7299UL, 0x273e662cUL, 0x17afbd17UL, 0x579b4468UL, 0x98f54449UL, 0x2c7d1bd9UL, 0x5c8a5fb4UL, 0x9a3bc004UL, 0x39296a78UL, 0x00000118UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Z      { 0x00000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_order_n  { 0x91386409UL, 0xBB6FB71EUL, 0x899C47AEUL, 0x3BB5C9B8UL, 0xF709A5D0UL, 0x7FCC0148UL, 0xBF2F966BUL, 0x51868783UL, 0xFFFFFFFAUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0xFFFFFFFFUL, 0x000001FFUL, 0x00000000UL }
#    define EscEcc_MY_P                { 0x00000000UL, 0x00004000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00800000UL }
#    define EscEcc_MY_N                { 0xf501c8d1UL, 0xe6fdc408UL, 0x12385bb1UL, 0xee145124UL, 0x8d91dd98UL, 0x968bf112UL, 0xffadc23dUL, 0x1a65200cUL, 0x5e1f1034UL, 0x00016b9eUL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00800000UL }
#    define EscEcc_PLEN 17U
#    define EscEcc_NLEN 17U
    /* EscEcc_KEY_LENGTH == 521 */
#   else
#    error "ECC key length must be defined to 160U, 192U, 224U, 256U, 384U or 521U for secPr1 curves"
#   endif
#elif ( EscEcc_CURVE_TYPE == EscEcc_brainpoolPr1 )

#   if ( EscEcc_KEY_LENGTH == 160U )
/***************************************************************************
 * 4.7 brainpoolP160r1 *
 ***************************************************************************/
/* UINT32 arrays */
#    define EscEcc_PRIME_P            { 0x9515620FUL, 0x95B3D813UL, 0x60DFC7ADUL, 0x737059DCUL, 0xE95E4A5FUL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_A      { 0xE8F7C300UL, 0xDA745D97UL, 0xE2BE61BAUL, 0xA280EB74UL, 0x340E7BE2UL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_B      { 0xD8675E58UL, 0xBDEC95C8UL, 0x134FAA2DUL, 0x95423412UL, 0x1E589A85UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_X     { 0xBDBCDBC3UL, 0x31EB5AF7UL, 0x62938C46UL, 0xEA3F6A4FUL, 0xBED5AF16UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Y     { 0x16DA6321UL, 0x669C9763UL, 0x38F94741UL, 0x7A1A8EC3UL, 0x1667CB47UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Z     { 0x00000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_order_n { 0x9E60FC09UL, 0xD4502940UL, 0x60DF5991UL, 0x737059DCUL, 0xE95E4A5FUL, 0x00000000UL }
#    define EscEcc_MY_P               { 0xc26fb1efUL, 0xa5d79737UL, 0xf1269ff8UL, 0x86396600UL, 0x18d392edUL, 0x00000001UL }
#    define EscEcc_MY_N               { 0x033bc7e6UL, 0xb54e9909UL, 0xf1272478UL, 0x86396600UL, 0x18d392edUL, 0x00000001UL }
#    define EscEcc_PLEN 5U
#    define EscEcc_NLEN 5U
    /* EscEcc_KEY_LENGTH == 160 */
#   elif ( EscEcc_KEY_LENGTH == 192U )
/***************************************************************************
 * 4.8 brainpoolP192r1 *
 ***************************************************************************/
/* UINT32 arrays */
#    define EscEcc_PRIME_P             { 0xE1A86297UL, 0x8FCE476DUL, 0x93D18DB7UL, 0xA7A34630UL, 0x932A36CDUL, 0xC302F41DUL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_A       { 0xC69A28EFUL, 0xCAE040E5UL, 0xFE8685C1UL, 0x9C39C031UL, 0x76B1E0E1UL, 0x6A911740UL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_B       { 0x6FBF25C9UL, 0xCA7EF414UL, 0x4F4496BCUL, 0xDC721D04UL, 0x7C28CCA3UL, 0x469A28EFUL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_X      { 0x53375FD6UL, 0x0A2F5C48UL, 0x6CB0F090UL, 0x53B033C5UL, 0xAAB6A487UL, 0xC0A0647EUL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Y      { 0xFA299B8FUL, 0xE6773FA2UL, 0xC1490002UL, 0x8B5F4828UL, 0x6ABD5BB8UL, 0x14B69086UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Z      { 0x00000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_order_n  { 0x9AC4ACC1UL, 0x5BE8F102UL, 0x9E9E916BUL, 0xA7A3462FUL, 0x932A36CDUL, 0xC302F41DUL, 0x00000000UL }
#    define EscEcc_MY_P                { 0x10b1ac0aUL, 0x6639fecfUL, 0xc2462077UL, 0x675bc2fdUL, 0xff1728c8UL, 0x500fea39UL, 0x00000001UL }
#    define EscEcc_MY_N                { 0x5e71f108UL, 0x3a674578UL, 0x68d2f9a8UL, 0x675bc2ffUL, 0xff1728c8UL, 0x500fea39UL, 0x00000001UL }
#    define EscEcc_PLEN 6U
#    define EscEcc_NLEN 6U
    /* EscEcc_KEY_LENGTH == 192 */
#   elif ( EscEcc_KEY_LENGTH == 224U )
/***************************************************************************
 * 4.9 brainpoolP224r1 *
 ***************************************************************************/
/* UINT32 arrays */
#    define EscEcc_PRIME_P             { 0x7EC8C0FFUL, 0x97DA89F5UL, 0xB09F0757UL, 0x75D1D787UL, 0x2A183025UL, 0x26436686UL, 0xD7C134AAUL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_A       { 0xCAD29F43UL, 0xB0042A59UL, 0x4E182AD8UL, 0xC1530B51UL, 0x299803A6UL, 0xA9CE6C1CUL, 0x68A5E62CUL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_B       { 0x386C400BUL, 0x66DBB372UL, 0x3E2135D2UL, 0xA92369E3UL, 0x870713B1UL, 0xCFE44138UL, 0x2580F63CUL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_X      { 0xEE12C07DUL, 0x4C1E6EFDUL, 0x9E4CE317UL, 0xA87DC68CUL, 0x340823B2UL, 0x2C7E5CF4UL, 0x0D9029ADUL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Y      { 0x761402CDUL, 0xCAA3F6D3UL, 0x354B9E99UL, 0x4ECDAC24UL, 0x24C6B89EUL, 0x72C0726FUL, 0x58AA56F7UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Z      { 0x00000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_order_n  { 0xA5A7939FUL, 0x6DDEBCA3UL, 0xD116BC4BUL, 0x75D0FB98UL, 0x2A183025UL, 0x26436686UL, 0xD7C134AAUL, 0x00000000UL }
#    define EscEcc_MY_P                { 0xb36f5f4fUL, 0xef60dc4dUL, 0x33da784fUL, 0x603de8fdUL, 0x4e1d543fUL, 0x8fd22299UL, 0x2fc099f7UL, 0x00000001UL }
#    define EscEcc_MY_N                { 0x1c8a5ba1UL, 0x590a94d3UL, 0xbee15bc3UL, 0x603f1e9fUL, 0x4e1d543fUL, 0x8fd22299UL, 0x2fc099f7UL, 0x00000001UL }
#    define EscEcc_PLEN 7U
#    define EscEcc_NLEN 7U
    /* EscEcc_KEY_LENGTH == 224 */
#   elif ( EscEcc_KEY_LENGTH == 256U )
/***************************************************************************
 * 4.10 brainpoolP256r1 *
 ***************************************************************************/
/* UINT32 arrays */
#    define EscEcc_PRIME_P             { 0x1F6E5377UL, 0x2013481DUL, 0xD5262028UL, 0x6E3BF623UL, 0x9D838D72UL, 0x3E660A90UL, 0xA1EEA9BCUL, 0xA9FB57DBUL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_A       { 0xF330B5D9UL, 0xE94A4B44UL, 0x26DC5C6CUL, 0xFB8055C1UL, 0x417AFFE7UL, 0xEEF67530UL, 0xFC2C3057UL, 0x7D5A0975UL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_B       { 0xFF8C07B6UL, 0x6BCCDC18UL, 0x5CF7E1CEUL, 0x95841629UL, 0xBBD77CBFUL, 0xF330B5D9UL, 0xE94A4B44UL, 0x26DC5C6CUL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_X      { 0x9ACE3262UL, 0x3A4453BDUL, 0xE3BD23C2UL, 0xB9DE27E1UL, 0xFC81B7AFUL, 0x2C4B482FUL, 0xCB7E57CBUL, 0x8BD2AEB9UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Y      { 0x2F046997UL, 0x5C1D54C7UL, 0x2DED8E54UL, 0xC2774513UL, 0x14611DC9UL, 0x97F8461AUL, 0xC3DAC4FDUL, 0x547EF835UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Z      { 0x00000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_order_n  { 0x974856A7UL, 0x901E0E82UL, 0xB561A6F7UL, 0x8C397AA3UL, 0x9D838D71UL, 0x3E660A90UL, 0xA1EEA9BCUL, 0xA9FB57DBUL, 0x00000000UL }
#    define EscEcc_MY_P                { 0x1180dd0cUL, 0xb62ae630UL, 0xff6a2fa9UL, 0x9b4f54a0UL, 0x322a7bf2UL, 0xbb73aba8UL, 0xa1c55b7eUL, 0x818c1131UL, 0x00000001UL }
#    define EscEcc_MY_N                { 0xccd10716UL, 0x50d73b46UL, 0x5fdf55eaUL, 0x9bf0088cUL, 0x322a7bf4UL, 0xbb73aba8UL, 0xa1c55b7eUL, 0x818c1131UL, 0x00000001UL }
#    define EscEcc_PLEN 8U
#    define EscEcc_NLEN 8U
    /* EscEcc_KEY_LENGTH == 256 */
#   elif ( EscEcc_KEY_LENGTH == 320U )
/***************************************************************************
 * 4.11 brainpoolP320r1 *
 ***************************************************************************/
/* UINT32 arrays */
#    define EscEcc_PRIME_P             { 0xF1B32E27UL, 0xFCD412B1UL, 0x7893EC28UL, 0x4F92B9ECUL, 0xF6F40DEFUL, 0xF98FCFA6UL, 0xD201E065UL, 0xE13C785EUL, 0x36BC4FB7UL, 0xD35E4720UL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_A       { 0x7D860EB4UL, 0x92F375A9UL, 0x85FFA9F4UL, 0x66190EB0UL, 0xF5EB79DAUL, 0xA2A73513UL, 0x6D3F3BB8UL, 0x83CCEBD4UL, 0x8FBAB0F8UL, 0x3EE30B56UL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_B       { 0x8FB1F1A6UL, 0x6F5EB4ACUL, 0x88453981UL, 0xCC31DCCDUL, 0x9554B49AUL, 0xE13F4134UL, 0x40688A6FUL, 0xD3AD1986UL, 0x9DFDBC42UL, 0x52088394UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_X      { 0x39E20611UL, 0x10AF8D0DUL, 0x10A599C7UL, 0xE7871E2AUL, 0x0A087EB6UL, 0xF20137D1UL, 0x8EE5BFE6UL, 0x5289BCC4UL, 0xFB53D8B8UL, 0x43BD7E9AUL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Y      { 0x692E8EE1UL, 0xD35245D1UL, 0xAAAC6AC7UL, 0xA9C77877UL, 0x117182EAUL, 0x0743FFEDUL, 0x7F77275EUL, 0xAB409324UL, 0x45EC1CC8UL, 0x14FDD055UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Z      { 0x00000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_order_n  { 0x44C59311UL, 0x8691555BUL, 0xEE8658E9UL, 0x2D482EC7UL, 0xB68F12A3UL, 0xF98FCFA5UL, 0xD201E065UL, 0xE13C785EUL, 0x36BC4FB7UL, 0xD35E4720UL, 0x00000000UL }
#    define EscEcc_MY_P                { 0xddc4b621UL, 0x2d8c7cafUL, 0x3d5ab45aUL, 0x55d42a20UL, 0x2237985cUL, 0x22b851a5UL, 0x89ad9837UL, 0x4195c155UL, 0xaf1aa120UL, 0x360e55a5UL, 0x00000001UL }
#    define EscEcc_MY_N                { 0xafa14203UL, 0x059081eaUL, 0xa154e856UL, 0x80461c1bUL, 0xf8341fe6UL, 0x22b851a6UL, 0x89ad9837UL, 0x4195c155UL, 0xaf1aa120UL, 0x360e55a5UL, 0x00000001UL }
#    define EscEcc_PLEN 10U
#    define EscEcc_NLEN 10U
    /* EscEcc_KEY_LENGTH == 320 */
#   elif ( EscEcc_KEY_LENGTH == 384U )
/***************************************************************************
 * 4.12 brainpoolP384r1 *
 ***************************************************************************/
/* UINT32 arrays */
#    define EscEcc_PRIME_P             { 0x3107EC53UL, 0x87470013UL, 0x901D1A71UL, 0xACD3A729UL, 0x7FB71123UL, 0x12B1DA19UL, 0xED5456B4UL, 0x152F7109UL, 0x50E641DFUL, 0x0F5D6F7EUL, 0xA3386D28UL, 0x8CB91E82UL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_A       { 0x22CE2826UL, 0x04A8C7DDUL, 0x503AD4EBUL, 0x8AA5814AUL, 0xBA91F90FUL, 0x139165EFUL, 0x4FB22787UL, 0xC2BEA28EUL, 0xCE05AFA0UL, 0x3C72080AUL, 0x3D8C150CUL, 0x7BC382C6UL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_B       { 0xFA504C11UL, 0x3AB78696UL, 0x95DBC994UL, 0x7CB43902UL, 0x3EEB62D5UL, 0x2E880EA5UL, 0x07DCD2A6UL, 0x2FB77DE1UL, 0x16F0447CUL, 0x8B39B554UL, 0x22CE2826UL, 0x04A8C7DDUL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_X      { 0x47D4AF1EUL, 0xEF87B2E2UL, 0x36D646AAUL, 0xE826E034UL, 0x0CBD10E8UL, 0xDB7FCAFEUL, 0x7EF14FE3UL, 0x8847A3E7UL, 0xB7C13F6BUL, 0xA2A63A81UL, 0x68CF45FFUL, 0x1D1C64F0UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Y      { 0x263C5315UL, 0x42820341UL, 0x77918111UL, 0x0E464621UL, 0xF9912928UL, 0xE19C054FUL, 0xFEEC5864UL, 0x62B70B29UL, 0x95CFD552UL, 0x5CB1EB8EUL, 0x20F9C2A4UL, 0x8ABE1D75UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Z      { 0x00000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_order_n  { 0xE9046565UL, 0x3B883202UL, 0x6B7FC310UL, 0xCF3AB6AFUL, 0xAC0425A7UL, 0x1F166E6CUL, 0xED5456B3UL, 0x152F7109UL, 0x50E641DFUL, 0x0F5D6F7EUL, 0xA3386D28UL, 0x8CB91E82UL, 0x00000000UL }
#    define EscEcc_MY_P                { 0x84a26716UL, 0x10a03bf6UL, 0x7a71566fUL, 0x9047bce0UL, 0xf1c4d721UL, 0x9ed590ceUL, 0xcae56edeUL, 0xdda2c449UL, 0x3cc6fa65UL, 0xff25adfdUL, 0x6d8ec6b8UL, 0xd1b575b1UL, 0x00000001UL }
#    define EscEcc_MY_N                { 0xf8a71f8aUL, 0x600adcccUL, 0x7a652109UL, 0x189fdb46UL, 0x165031e7UL, 0xc506f2feUL, 0xcae56ee1UL, 0xdda2c449UL, 0x3cc6fa65UL, 0xff25adfdUL, 0x6d8ec6b8UL, 0xd1b575b1UL, 0x00000001UL }
#    define EscEcc_PLEN 12U
#    define EscEcc_NLEN 12U
    /* EscEcc_KEY_LENGTH == 384 */
#   elif ( EscEcc_KEY_LENGTH == 512U )
/***************************************************************************
 * 4.13 brainpoolP512r1 *
 ***************************************************************************/
/* UINT32 arrays */
#    define EscEcc_PRIME_P             { 0x583A48F3UL, 0x28AA6056UL, 0x2D82C685UL, 0x2881FF2FUL, 0xE6A380E6UL, 0xAECDA12AUL, 0x9BC66842UL, 0x7D4D9B00UL, 0x70330871UL, 0xD6639CCAUL, 0xB3C9D20EUL, 0xCB308DB3UL, 0x33C9FC07UL, 0x3FD4E6AEUL, 0xDBE9C48BUL, 0xAADD9DB8UL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_A       { 0x77FC94CAUL, 0xE7C1AC4DUL, 0x2BF2C7B9UL, 0x7F1117A7UL, 0x8B9AC8B5UL, 0x0A2EF1C9UL, 0xA8253AA1UL, 0x2DED5D5AUL, 0xEA9863BCUL, 0xA83441CAUL, 0x3DF91610UL, 0x94CBDD8DUL, 0xAC234CC5UL, 0xE2327145UL, 0x8B603B89UL, 0x7830A331UL, 0x00000000UL }
#    define EscEcc_COEFFICIENT_B       { 0x8016F723UL, 0x2809BD63UL, 0x5EBAE5DDUL, 0x984050B7UL, 0xDC083E67UL, 0x77FC94CAUL, 0xE7C1AC4DUL, 0x2BF2C7B9UL, 0x7F1117A7UL, 0x8B9AC8B5UL, 0x0A2EF1C9UL, 0xA8253AA1UL, 0x2DED5D5AUL, 0xEA9863BCUL, 0xA83441CAUL, 0x3DF91610UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_X      { 0xBCB9F822UL, 0x8B352209UL, 0x406A5E68UL, 0x7C6D5047UL, 0x93B97D5FUL, 0x50D1687BUL, 0xE2D0D48DUL, 0xFF3B1F78UL, 0xF4D0098EUL, 0xB43B62EEUL, 0xB5D916C1UL, 0x85ED9F70UL, 0x9C4C6A93UL, 0x5A21322EUL, 0xD82ED964UL, 0x81AEE4BDUL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Y      { 0x3AD80892UL, 0x78CD1E0FUL, 0xA8F05406UL, 0xD1CA2B2FUL, 0x8A2763AEUL, 0x5BCA4BD8UL, 0x4A5F485EUL, 0xB2DCDE49UL, 0x881F8111UL, 0xA000C55BUL, 0x24A57B1AUL, 0xF209F700UL, 0xCF7822FDUL, 0xC0EABFA9UL, 0x566332ECUL, 0x7DDE385DUL, 0x00000000UL }
#    define EscEcc_BASE_POINT_G_Z      { 0x00000001UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL, 0x00000000UL }
#    define EscEcc_BASE_POINT_order_n  { 0x9CA90069UL, 0xB5879682UL, 0x085DDADDUL, 0x1DB1D381UL, 0x7FAC1047UL, 0x41866119UL, 0x4CA92619UL, 0x553E5C41UL, 0x70330870UL, 0xD6639CCAUL, 0xB3C9D20EUL, 0xCB308DB3UL, 0x33C9FC07UL, 0x3FD4E6AEUL, 0xDBE9C48BUL, 0xAADD9DB8UL, 0x00000000UL }
#    define EscEcc_MY_P                { 0xe911e8d9UL, 0x17e2cf84UL, 0x603556d1UL, 0x71d621c4UL, 0x4e73ea8cUL, 0xe47d9303UL, 0x823152c5UL, 0x42ff2b38UL, 0xf5bf92f5UL, 0x666ad8f2UL, 0xcc44ef09UL, 0x8373af60UL, 0x03461e1eUL, 0x15d5ea2fUL, 0xd6daeb8aUL, 0x7f8d7f4eUL, 0x00000001UL }
#    define EscEcc_MY_N                { 0xdb57db37UL, 0x2fafac64UL, 0x15d5c4ceUL, 0x0eaf0d90UL, 0x59ee4710UL, 0x9ff38f5fUL, 0x1a235d44UL, 0xdb9470c6UL, 0xf5bf92f7UL, 0x666ad8f2UL, 0xcc44ef09UL, 0x8373af60UL, 0x03461e1eUL, 0x15d5ea2fUL, 0xd6daeb8aUL, 0x7f8d7f4eUL, 0x00000001UL }
#    define EscEcc_PLEN 16U
#    define EscEcc_NLEN 16U
    /* EscEcc_KEY_LENGTH == 512 */
#   else
#    error "ECC key length must be defined to 160U, 192U, 224U, 256U, 320U or 512U for brainpoolPr1 curves"
#   endif /* EscEcc_KEY_LENGTH  */

#else
# error "Not supported EscEcc_CURVE_TYPE!"
#endif /* EscEcc_CURVE_TYPE */

/***************************************************************************
 * 3. DEFINITIONS                                                          *
 ***************************************************************************/

/***************************************************************************
 * 4. CONSTANTS                                                            *
 ***************************************************************************/

/* secp curve (define ecc field parameter before first use!) */
const EscEcc_CurveT EscEcc_curve32 = {
    { EscEcc_COEFFICIENT_A },       /* coefficient_A       */
    { EscEcc_COEFFICIENT_B },       /* coefficient_B       */
    {
        C_PT_JACOBIAN,               /* base point G type   */
        { EscEcc_BASE_POINT_G_X },     /* base point G->x     */
        { EscEcc_BASE_POINT_G_Y },     /* base point G->y     */
        { EscEcc_BASE_POINT_G_Z },     /* base point G->z     */
    },
    {
        /* base_point_order_n  */
        { EscEcc_MY_N },               /* precalculated my_n, used in barrett reduction  */
        { EscEcc_BASE_POINT_order_n }, /* BASE_POINT          */
        EscEcc_NLEN
    },
    1U,                         /* cofactor_h          */
    {
        { EscEcc_MY_P },               /* precalculated my_p, used in barrett reduction  */
        { EscEcc_PRIME_P },            /* PRIME_P             */
        EscEcc_PLEN
    }
};

/***************************************************************************
 * 5. IMPLEMENTATION OF FUNCTIONS                                          *
 ***************************************************************************/

/***************************************************************************
 * 6. END                                                                  *
 ***************************************************************************/
