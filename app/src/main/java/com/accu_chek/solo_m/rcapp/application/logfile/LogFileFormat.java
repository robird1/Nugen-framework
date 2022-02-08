/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogFileFormat
 * Brief: The interface of the log log file format.
 *
 * Create Date: 07/15/2015
 * $Revision: 21903 $
 * $Author: WilliyChiang $
 * $Id: LogFileFormat.java 21903 2015-10-19 07:31:17Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;

import java.io.File;
import java.io.RandomAccessFile;

public interface LogFileFormat
{
    
    /**
     * Set the position of a log in the log file.
     * 
     * @param logFile The File object of the log file.
     *       Range: Valid File object
     *       Unit: File
     *       Scaling: 1
     *       
     * @param randomAccessFile The RandomAccessFile object to access the log file.
     *       Range: Valid RandomAccessFile object
     *       Unit: RandomAccessFile
     *       Scaling: 1
     *       
     * @param endTag The SafetyString object of the log end tag.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @param necessaryPosstion The position number in HeadPosition.
     *       Range: 0 ~ (2 ^ 31)-1
     *       Unit: int
     *       Scaling: 1
     *       
     * @return The result
     *       Range: 0 ~ (2 ^ 63)-1
     *       Unit: long
     *       Scaling: 1
     */
    long setStartPosition(File logFile, RandomAccessFile randomAccessFile,
            SafetyString endTag, int... necessaryPosstion);

    /**
     * Transfer a log into correct format.
     * 
     * @param log The SafetyString of a log.
     *       Range: null, not null
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @return The result
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     */
    SafetyString getContextwithFormat(SafetyString log);

    /**
     * Get an end tag string with correct format.
     * 
     * @param endTag The SafetyString of end tag.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
      * @param necessaryChar The SafetyString of auxiliary text.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @return The result
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     */
    SafetyString getEndwithFormat(SafetyString endTag,
            SafetyString... necessaryChar);

}
