/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception.
 * BgmCommunicationTimeOut
 * Brief:
 *
 * Create Date: 2015¦~11¤ë16¤é
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception;

public class BgmCommunicationTimeOutException extends Exception
{

    /**
     * 
     */
    private static final long serialVersionUID = 3684016983604248583L;


    /**
     * default exception message
     */
    private static final String DEFAULT_MESSAGE = "BgmCommunication TimeOut";
    /**
     * Construct a new BgmCRCDataErrorException with the current stack trace and
     * the default message.
     * return void [out] None.
     */
    public BgmCommunicationTimeOutException()
    {
        super(DEFAULT_MESSAGE);
    }

}
// Refine code comment.
