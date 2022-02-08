/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 * 
 * Class name: com.accu_chek.cgmapp.application.exception.DataIntegrityException
 * Brief: Thrown when there is data integrity problem happened
 * 
 * Create Date: 2014/1/6
 * $Revision: 20527 $
 * $Author: DWYang $
 * $Id: DataIntegrityException.java 20527 2015-10-01 11:17:29Z DWYang $
 */

package com.accu_chek.solo_m.rcapp.application.exception;

public class DataIntegrityException extends RuntimeException
{
    /**
     * generated serial version id
     * This ID is generaled by Android. DON't change to upper case
     */
    private static final long serialVersionUID = -7029069200167454638L;

    /**
     * default exception message
     */
    private static final String DEFAULT_MESSAGE = "Data integrity exception";

    /**
     * Constructs a new DataIntegrityException with the current stack trace and
     * the default message.
     */
    public DataIntegrityException()
    {
        super(DEFAULT_MESSAGE);
    }

    /**
     * Constructs a new DataIntegrityException with the current stack trace and
     * the specified detail message.
     * 
     * @param detailMessage [in] the detail message for this exception.
     */
    public DataIntegrityException(String detailMessage)
    {
        super(detailMessage);
    }

    /**
     * Constructs a new DataIntegrityException with the current stack trace and
     * the specified cause.
     * 
     * @param exception [in] the cause of this exception.
     */
    public DataIntegrityException(Exception exception)
    {
        super(exception);
    }
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */
// add comments
// (3337 2014-01-06 06:13:06Z PhoenixCheng)
// ----------------------------------------------------------------------------
// Restore the sources to revision 3682.
// (3700 2014-01-22 08:38:45Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: N/A
// [Comment]: Undone KEYWORD function of SVN. No Source code content will be
// changed
// (3700 2014-01-22 08:38:45Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: N/A
// [Comment]: Preform KEYWORD function of SVN for each source files. No Source
// code content will be changed
// (21021 2014-10-03 02:34:45Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Comply coding guideline
// (21467 2014-10-07 14:22:36Z HenryTso)
// ----------------------------------------------------------------------------
// [JIRA-ID]: ATOS-267
// [Comment]: Update for comply the coding guideline
