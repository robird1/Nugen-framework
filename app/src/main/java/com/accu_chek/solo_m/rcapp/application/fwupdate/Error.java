/** 
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: ${package_name}.${type_name}
 * Brief: 
 *
 * Create Date: ${date}
 * $$Revision: 22760 $$
 * $$Author: JensonChin $$
 * $$Id: FWUpdate.java 22760 2015-10-28 15:07:17Z JensonChin $$
 */
package com.accu_chek.solo_m.rcapp.application.fwupdate;

import com.accu_chek.solo_m.rcapp.application.common.HammingValues;

/**
 *  Module internal/external error code
 */
public interface Error
{
    public static final int ERR_OK = HammingValues.HAMMING_HD4_VALUE_0001;
    public static final int ERR_UART_RELESE_FAIL = HammingValues.HAMMING_HD4_VALUE_0002;
    public static final int ERR_COMMS_UPDATE_FAIL = HammingValues.HAMMING_HD4_VALUE_0003;
    public static final int ERR_MAIN_UPDATE_FAIL = HammingValues.HAMMING_HD4_VALUE_0004;
    public static final int ERR_FILE_COPY_FAIL = HammingValues.HAMMING_HD4_VALUE_0005;
    public static final int ERR_THREAD_EXE_FAIL = HammingValues.HAMMING_HD4_VALUE_0006;
    public static final int ERR_UPDATE_STATUS_FAIL = HammingValues.HAMMING_HD4_VALUE_0007;
    public static final int ERR_EXIT_FAIL = HammingValues.HAMMING_HD4_VALUE_0008;
    public static final int ERR_RESUME_FAIL = HammingValues.HAMMING_HD4_VALUE_0009;
    public static final int ERR_ZIP_EOF = HammingValues.HAMMING_HD4_VALUE_0010;
}