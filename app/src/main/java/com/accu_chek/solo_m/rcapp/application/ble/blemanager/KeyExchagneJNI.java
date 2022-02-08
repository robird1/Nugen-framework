/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.comms.ble.KeyExchagneJNI
 * Brief: The class handles the KES JNI function.
 *
 * Create Date: 2015/7/21
 * $Revision: 25164 $
 * $Author: KiddYeh $
 * $Id: KeyExchagneJNI.java 25164 2015-11-30 10:20:58Z KiddYeh $
 */
package com.accu_chek.solo_m.rcapp.application.ble.blemanager;

public final class KeyExchagneJNI
{
    public static native void Kes_test();
    public static native void Kes_init(byte[] entropy, byte[] nonce);
    public static native byte[] Kes_step1(byte[] M1);
    public static native byte[] Kes_step2(byte[] M3, byte[] Pin);
    public static native byte[] Kes_step3(byte[] M5);
    public static native byte[] Kes_done();
}
/* ===========================================================================
 * 
 * Revision history
 * 
 * =========================================================================== */
// (R15977 2015-09-01 03:30:25 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] Add header and footer for BLEUC02.java
// (R19054 2015-09-21 05:39:27 KiddYeh)
// ----------------------------------------------------------------------------
// [BT] modified SetConfigration & added header/footer.
