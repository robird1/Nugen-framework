/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name:
 * com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception
 * .BgmCRCDataError
 * Brief:
 *
 * Create Date: 2015¦~4¤ë9¤é
 * $Revision: 24486 $
 * $Author: VictorChen $
 * $Id: BgmCRCDataErrorException.java 24486 2015-11-20 05:48:56Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception;

public class BgmCRCDataErrorException extends Exception
{

    /**
     * generated serial version id
     * This ID is generaled by Android. DON't change to upper case
     */
    private static final long serialVersionUID = -1631083723732092398L;

    /**
     * default exception message
     */
    private static final String DEFAULT_MESSAGE = "BGM check data CRC error";

 
    /**
     * Construct a new BgmCRCDataErrorException with the current stack trace and
     * the default message.
     */
    public BgmCRCDataErrorException()
    {
        super(DEFAULT_MESSAGE);
    }

}
// (R20520 2015-10-01 07:04:11 DWYang)
// ----------------------------------------------------------------------------
// Refine code comment.
