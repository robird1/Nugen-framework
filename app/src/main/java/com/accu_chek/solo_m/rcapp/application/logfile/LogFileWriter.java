/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogFileWriter
 * Brief: The class is the writer to log file
 *
 * Create Date: 07/15/2015
 * $Revision: 21903 $
 * $Author: WilliyChiang $
 * $Id: LogFileWriter.java 21903 2015-10-19 07:31:17Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import android.content.Context;
import android.util.Xml;

import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.FileLockInterruptionException;

import org.xmlpull.v1.XmlSerializer;

public final class LogFileWriter
{
    
    /**
     * The initial value of a byte value.
     */
    private static final byte BYTE_FIELD_UNUSED = -1;
  
    /**
     * The directory of the log file.
     */
    private static final String ROOT_DIR = LogFileConstants.LOG_PATH;

    /**
     * The name of the log file.
     */
    private static final String LOGFILENAME = "log";
    
    /**
     * The default message for the RC application log and the RC log.
     */
    private static final String DEFAULTMESSAGE = "No Info %-492d";
 
    /**
     * The XML encoding of the log file.
     */
    private static final String XML_ENCODING = "ISO-8859-1";
    
    /**
     * The slash symbol.
     */   
    private static final String SLASH = "/";
    
    /**
     * The access mode of the log file.
     */ 
    private static final String FILE_ACCESS_MODE = "rw";
    
    /**
     * The LogFileWriter instance in this class.
     */
    private static LogFileWriter mLogFileWriter = null;

    /**
     * The global variable is used to record whether the log file is initialized.
     */
    private SafetyBoolean mIsInitialized = SafetyBoolean.FALSE;

    /**
     * The File instance for the log file.
     */
    private File mLogFile = null;

    /**
     * The global variable is the Lock instance which is used to synchronize the log file.
     */
    private Object mLock = new Object();

    /**
     * The context of the LogFileWriter class.
     */
    private Context mContext = null;
    
    /**
     * The global variable is used to log the errors of the LogFileWriter class.
     */
    private LogLogFileError mLogFileError = new LogLogFileError();

    /**
     * The constructor of LogFileWriter. Initialize the global variables and create file
     * directories if the directories do not exist.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     */
    LogFileWriter(final Context context)
    {
        // Create a new file
        final File rootDir = new File(ROOT_DIR);
        final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW47306);
        
        final boolean isRootrDirExist = rootDir.exists();

        // Set mContext to context
        mContext = context.getApplicationContext();

        // Does the root directory exist?
        if (!isRootrDirExist)
        {
            // Show EMWR
            NotifyProxy.showEMWR(emwrMessage);
        }
    }

    /**
     * Set the initial flag of the log file.
     * 
     * @param isInitedFlag The initial flag of the log file.
     *       Range: Valid SafetyBoolean object
     *       Unit: SafetyBoolean
     *       Scaling: 1
     *       
     * @return None
     */
    void setInitialFlag(final SafetyBoolean isInitedFlag)
    {
        // Set mIsInitialized to isInitedFlag
        mIsInitialized = isInitedFlag;
    }

    /**
     * Get the File object of the log file.
     * 
     * @param None
     * 
     * @return The result
     *       Range: Valid File object
     *       Unit: File
     *       Scaling: 1
     */
    File getLogFile()
    {
        synchronized (mLock)
        {
            // Return mErrorlogfile
            return mLogFile;
        }
    }

    /**
     * Check whether the log file is created or not.
     * Create a new log file if it does not exist.
     * 
     * @param None
     * 
     * @return None
     */
    void initFromDic()
    {
        synchronized (mLock)
        {
            final byte byteOfIsInitialized = mIsInitialized.getByte();
            final byte byteOfIsFalseBoolean = SafetyBoolean.FALSE.getByte();
            final byte byteOfSearchFileResult = searchFile().getByte();
            
            // Is mIsInitialized equal to SafetyBoolean.FALSE?
            if (byteOfIsInitialized == byteOfIsFalseBoolean)
            {
                // Search log file on the phone
                if (byteOfSearchFileResult == byteOfIsFalseBoolean)
                {
                    // Create a new log file
                    createSystemLogFile();
                }

                // Set mIsInitialized to SafetyBoolean.TRUE
                mIsInitialized = SafetyBoolean.TRUE;
            }
        }
    }

    /**
     * Create a new log file.
     * The function refreshes MTP database when a new log file was created (Refresh MTP
     * database to make sure the file on a computer immediately).
     * And it calls initialNewFile to write default head message into
     * the log file.
     * 
     * @param None
     * 
     * @return None
     */
    void createSystemLogFile()
    {
        synchronized (mLock)
        {
            // Configure mErrorlogfile
            mLogFile = new File(ROOT_DIR + SLASH + LOGFILENAME);

            // Fill headers into the new log file.
            initialNewFile();
        }
    }
    
    /**
     * Write a log into log file. There are three writing styles:
     * 1. Initial format: put head lines into log file.
     * 2. Replaced format: replace specific line in log file.
     * 3. Append format: append new line into log file.
     * 
     * Different formats have different start position, content position and end
     * line.
     * 
     * In order to avoid from writing by different processes at same time,
     * FileLock (Android API) is used to lock the file when some process is
     * writing. In order to avoid from writing by different threads at same
     * time, synchronized object is used to make sure only one thread enter.
     * 
     * After writing, the function will release FileLock and synchronized
     * object.
     * 
     * @param logData The log data that includes log's time and log's content.
     *       Range: Valid LogData object
     *       Unit: LogData
     *       Scaling: 1
     *       
     * @param isAppend Check whether the log is appended into the head of the log file
     *                 or the log of the log file.      
     *       Range: Valid SafetyBoolean object
     *       Unit: SafetyBoolean
     *       Scaling: 1
     *       
     * @param position The position in HeadPosition in the log file.
     *       Range: 0 ~ (2 ^ 31)-1
     *       Unit: int
     *       Scaling: 1
     *       
     * @return The result of the writing process. If the return value is equal to 
     *       SafetyBoolean.TRUE, it means the writing process is successful.
     *       If the return value is equal to SafetyBoolean.FALSE, it means the
     *       writing process is failed.
     *       Range: Valid SafetyBoolean object
     *       Unit: SafetyBoolean
     *       Scaling: 1
     */
    SafetyBoolean writeSystemLog(final LogData logData, final SafetyBoolean isAppend,
            final int... position)
    {
        SafetyBoolean isSuccessfulWriting = SafetyBoolean.FALSE;

        synchronized (mLock)
        {
            // Is mLogFile equal to null?
            if (mLogFile != null)
            {
                RandomAccessFile fileOutput = null;
                FileLock fileLock = null;
                String logContent = null;
                int byteNumber = 0;
                final StringBuffer stringBuffer = new StringBuffer();
                
                // Is logData equal to null?
                if (logData != null)
                {
                    // Get the content of logData
                    logContent = logData.getContent();

                    // Is the content of logData equal to null?
                    if (logContent != null)
                    {
                        // Get the content length of logData
                        byteNumber = logContent.getBytes().length;
                    }
                }

                try 
                {
                    // Is the content length of logData larger than 0?
                    if (byteNumber > 0)
                    {
                        File tempFile = null;
                        boolean isSameFile = true;

                        do
                        {
                            //Close current log file and release the file lock of the log file.
                            closeFileAndReleaseFileLock(fileOutput, fileLock);

                            tempFile = mLogFile;

                            // Get a new RandomAccessFile base on mLogFile
                            fileOutput = new RandomAccessFile(mLogFile, FILE_ACCESS_MODE);
                            
                            // Lock the RandomAccessFile base on mLogFile
                            fileLock = getFileMutex(fileOutput.getChannel());

                            // Search the log file on the device.
                            searchFile();

                            // Is mLogFile equal to null?
                            if (mLogFile == null)
                            {
                                // Create a new log file
                                createSystemLogFile();
                            }
                            else
                            {
                                // Compare the paths between the searched file and mLogFile
                                isSameFile = tempFile.getAbsolutePath().equals(
                                        mLogFile.getAbsolutePath());
                            }

                        } while (isSameFile == false);

                        // Write the log into the log file, and get the writing result
                        isSuccessfulWriting = writeLog(stringBuffer, fileOutput, fileLock, isAppend, 
                                logContent, position);
                    }
                }
                catch (Exception exception)
                {  
                    // Write the exception into a file
                    mLogFileError.write(null, exception);
                }
                finally
                {
                    // Close current log file and release the file lock of the log file
                    closeFileAndReleaseFileLock(fileOutput, fileLock);

                    // Clear string buffer
                    stringBuffer.delete(0, stringBuffer.length());
                }
            }
        }

        /*
         * Return the result. If the return value is equal to SafetyBoolean.TRUE,
         * it means the writing process is successful. If the return value is 
         * equal to SafetyBoolean.FALSE, it means the writing process is failed.
         */
        return isSuccessfulWriting;
    }
    
    /**
     * Write a log into the log file.
     * 
     * @param stringBuffer The StringBuffe object that stores the log content.
     *       Range: Valid StringBuffer object
     *       Unit: StringBuffer
     *       Scaling: 1
     * 
     * @param fileOutput The RandomAccessFile object that is used to access to the log file.
     *       Range: Valid RandomAccessFile object
     *       Unit: RandomAccessFile
     *       Scaling: 1
     * 
     * @param fileLock The FileLock object that is used to lock the log file.
     *       Range: Valid FileLock object
     *       Unit: FileLock
     *       Scaling: 1
     *       
     * @param isAppend Check whether the log is appended into the head of the log file
     *                 or the log of the log file.      
     *       Range: Valid SafetyBoolean object
     *       Unit: SafetyBoolean
     *       Scaling: 1
     *    
     * @param logContent The log content
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1     
     *       
     * @param position The position in HeadPosition in the log file.
     *       Range: 0 ~ (2 ^ 31)-1
     *       Unit: int
     *       Scaling: 1
     *       
     * @return The result
     *       Range: Valid SafetyBoolean object
     *       Unit: SafetyBoolean
     *       Scaling: 1
     */    
    private SafetyBoolean writeLog(final StringBuffer stringBuffer, 
            final RandomAccessFile fileOutput, final FileLock fileLock, 
            final SafetyBoolean isAppend, final String logContent,
            final int... position)
    {
        SafetyBoolean isSuccessfulWriting = SafetyBoolean.FALSE;
        SafetyString endstring = null;
        SafetyString newLine = null;
        LogFileFormat fileFormat = null;
     
        synchronized (mLock)
        {        
            // Is fileLock equal to null?
            if (fileLock != null)
            {
                // Get the safety string of "</Log>"
                endstring = new SafetyString(LogFileConstants.END_TAG_LOG,
                        CRCTool.generateCRC16(LogFileConstants.END_TAG_LOG.getBytes()));
                
                // Get the safety string of "\r\n"
                newLine = new SafetyString(LogFileConstants.NEW_LINE_CHARACTORS,
                        CRCTool.generateCRC16(LogFileConstants.NEW_LINE_CHARACTORS.getBytes()));
    
                // Get the format of the logContent
                fileFormat = getLogFileFormat(isAppend, position);
    
                // Set the position of the log content in the log file
                fileFormat.setStartPosition(mLogFile,
                        fileOutput, endstring, position);
    
                // Append the content of logData
                stringBuffer.append(logContent);
    
                // Write the log Content into the log file
                writeLogContent(fileOutput, fileFormat, stringBuffer.toString());
    
                // Write the end tag into the log file
                writeLogEnd(fileOutput, fileFormat, endstring,
                        newLine);
    
                // Close current log file and release the file lock of the log file.
                closeFileAndReleaseFileLock(fileOutput, fileLock);
    
                // Set the result to SafetyBoolean.TRUE
                isSuccessfulWriting = SafetyBoolean.TRUE;
            }
        }
        
        /*
         * Return the result. If the return value is equal to SafetyBoolean.TRUE,
         * it means the writing process is successful. If the return value is 
         * equal to SafetyBoolean.FALSE, it means the writing process is failed.
         */
        return isSuccessfulWriting;
    }
    
    /**
     * Get the format for the log content. The format is used to create, append,
     * or replace the log content into the log file.
     *       
     * @param isAppend Check whether the log is appended into the head of the log file
     *                 or the log of the log file.      
     *       Range: Valid SafetyBoolean object
     *       Unit: SafetyBoolean
     *       Scaling: 1
     *       
     * @param position The position in HeadPosition in the log file.
     *       Range: 0 ~ (2 ^ 31)-1
     *       Unit: int
     *       Scaling: 1
     *       
     * @return The result
     *       Range: Valid SafetyBoolean object
     *       Unit: SafetyBoolean
     *       Scaling: 1
     */    
    private LogFileFormat getLogFileFormat(final SafetyBoolean isAppend,
            final int... position)
    {
        final byte byteOfIsFalseBoolean = SafetyBoolean.FALSE.getByte();
        final byte byteOfIsTrueBoolean = SafetyBoolean.TRUE.getByte();
        byte byteOfIsCorrectPositionValue = BYTE_FIELD_UNUSED;
        final byte byteOfIsAppend = isAppend.getByte();
        LogFileFormat fileFormat = null;
        
        // Check whether position is correct
        byteOfIsCorrectPositionValue = checkPosition(position).getByte();
        
        // Is the log append?
        // Is the position invalid?
        if ((byteOfIsAppend == byteOfIsTrueBoolean)
                || (byteOfIsCorrectPositionValue == byteOfIsFalseBoolean))
        {
            // Create a LineAppendFormat object
            fileFormat = new LineAppendFormat();
        }
        // Is position equal to null?
        else if (position != null)
        {
            final int nLengthOfPosition = position.length;

            // Is the length of position equal to 0?
            if (nLengthOfPosition != 0)
            {
                // Create a LineReplaceFormat object
                fileFormat = new LineReplaceFormat();
            }
            else
            {
                // Create a LineInitialFormat object
                fileFormat = new LineInitialFormat();
            }
        }
        else
        {
            // Create a LineInitialFormat object
            fileFormat = new LineInitialFormat();
        }
        
        /*
         * Return the format for the log content. The format is used to create, append,
         * or replace the log content into the log file.
         */
        return fileFormat;
    }

    /**
     * Fill headers in the new log file.
     * 
     * @param None
     * 
     * @return None
     */
    private void initialNewFile()
    {
        final XmlSerializer xmlSerializer = Xml.newSerializer();
        final ByteArrayOutputStream writer = new ByteArrayOutputStream();
        final LogData logData = new LogData();
        final File rootDir = new File(ROOT_DIR);
        final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW47306);

        // Does the root directories exist?
        if (!rootDir.exists())
        {
            // Show EMWR
            NotifyProxy.showEMWR(emwrMessage);
        }
        else
        {
            try
            {
                // Set to use binary output stream with ISO-8859-1
                xmlSerializer.setOutput(writer, XML_ENCODING);
    
                // start DOCUMENT
                xmlSerializer.startDocument(XML_ENCODING, true);
                xmlSerializer.text(LogFileConstants.NEW_LINE_CHARACTORS);
                xmlSerializer.text(LogFileConstants.NEW_LINE_CHARACTORS);
    
                // Write the tag "Log"
                xmlSerializer.startTag(null, LogFileConstants.TAG_LOG);
                xmlSerializer.text(LogFileConstants.NEW_LINE_CHARACTORS);
    
                // Write the tag "Head"
                xmlSerializer.startTag(null, LogFileConstants.TAG_HEAD);
                xmlSerializer.text(LogFileConstants.NEW_LINE_CHARACTORS);
                xmlSerializer.flush();
    
                // Write the default RC application information to log file
                xmlSerializer.text(String.format(DEFAULTMESSAGE, 0));
                xmlSerializer.text(LogFileConstants.NEW_LINE_CHARACTORS);
                xmlSerializer.flush();
    
                // Write the default RC device information to log file
                xmlSerializer.text(String.format(DEFAULTMESSAGE, 0));
                xmlSerializer.text(LogFileConstants.NEW_LINE_CHARACTORS);
                
                // Write the end tag of head
                xmlSerializer.endTag(null, LogFileConstants.TAG_HEAD);
                xmlSerializer.flush();
    
                // Set the content of log data to the string of binary output stream
                logData.setContent(writer.toString());
    
                // Write the log data to log file
                writeSystemLog(logData, SafetyBoolean.FALSE);
                
                // Write the RC application information into the log file
                LogFile.getInstance().log(new LogAppInfo(mContext));
                
                // Write the RC device information into the log file
                LogFile.getInstance().log(new LogDeviceInfo(mContext));
            }
            catch (IllegalArgumentException exception)
            {
                // Write the error of the log file into a file
                mLogFileError.write(null, exception);
            }
            catch (IllegalStateException exception)
            {
                // Write the exception into a file
                mLogFileError.write(null, exception);
            }
            catch (IOException exception)
            {
                // Write the error of the log file into a file
                mLogFileError.write(null, exception);
            }
            finally
            {
                // Apply to the coding standard
            }
        }
    }

    /**
     * Lock the file channel to prevent other processes from writing the log file.
     * If the log file is not found, call initFromDic() to initialize the file.
     * 
     * @param fileChannel The FileChannel object for the log file.
     *       Range: Valid FileChannel object
     *       Unit: FileChannel
     *       Scaling: 1
     *       
     * @return The result
     *       Range: Valid FileLock object
     *       Unit: FileLock
     *       Scaling: 1
     */
    private FileLock getFileMutex(final FileChannel fileChannel)
    {
        FileLock fileLock = null;

        try
        {
            // Get a file lock on fileChannel
            fileLock = fileChannel.lock();
        }
        catch (FileNotFoundException exception)
        {
            // Write the exception into a file
            mLogFileError.write(null, exception);
        }
        catch (FileLockInterruptionException exception)
        {
            // Write the exception into a file
            mLogFileError.write(null, exception);
        }
        catch (IOException exception)
        {
            // Write the exception into a file
            mLogFileError.write(null, exception);
        }
        finally
        {
            // Apply to the coding standard
        }

        // Return the file lock
        return fileLock;
    }

    /**
     * Unlock the file channel if the file channel has been locked.
     * The function does nothing if the file is never locked.
     * 
     * @param fileLock The FileLock object for the log file.
     *       Range: Valid FileLock object
     *       Unit: FileLock
     *       Scaling: 1
     * 
     * @return None
     */
    private void releaseFileMutex(final FileLock fileLock)
    {
        // Is fileLock equal to null?
        if (fileLock != null)
        {
            final boolean isValidLock = fileLock.isValid();

            // Is fileLock valid?
            if (isValidLock == true)
            {
                try
                {
                    // Release fileLock
                    fileLock.release();
                }
                catch (IOException exception)
                {
                    // Write the exception into a file
                    mLogFileError.write(null, exception);
                }
                finally
                {
                    // Apply to the coding standard
                }
            }
        }
    }

    /**
     * Close the log file and release the file lock of the log file.
     * 
     * @param fileOutput The RandomAccessFile object to access the log file.
     *       Range: Valid RandomAccessFile object
     *       Unit: RandomAccessFile
     *       Scaling: 1
     * 
     * @param fileLock The FileLock object for the log file.
     *       Range: Valid FileLock object
     *       Unit: FileLock
     *       Scaling: 1
     *       
     * @return None
     */
    private void closeFileAndReleaseFileLock(final RandomAccessFile fileOutput,
            final FileLock fileLock)
    {
        // Is fileOutput equal to null?
        if (fileOutput != null)
        {
            try
            {
                // Close fileOutput 
                fileOutput.close();
            }
            catch (IOException exception)
            {
                // Write the exception into a file
                mLogFileError.write(null, exception);
            }
            finally
            {
                // Apply to the coding standard
            }
            
            // Unlock fileLock
            releaseFileMutex(fileLock);
        }
    }


    /**
     * Write a log into log file.
     * 
     * @param fileOutput The RandomAccessFile object that is used to access the log file.
     *       Range: Valid RandomAccessFile object
     *       Unit: RandomAccessFile
     *       Scaling: 1
     *       
     * @param fileFormat The LogFileFormat object for the log.
     *       Range: Valid LogFileFormat object
     *       Unit: LogFileFormat
     *       Scaling: 1
     *       
     * @param content The log content.
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @return None
     */
    private void writeLogContent(final RandomAccessFile fileOutput,
            final LogFileFormat fileFormat, final String content)
    {
        // Get the split string
        final SafetyString[] splitString = splitLog(content);

        SafetyString logString = null;

        // Are the split logs equal to null?
        if (splitString != null)
        {
            try
            {
                for (int i = 0; i < splitString.length; ++i)
                {
                    final long indexLastChar = splitString.length - 1;

                    // Get the formatted string of the current split log
                    logString = fileFormat.getContextwithFormat(splitString[i]);

                    // Is the formatted string of the current split log equal to null?
                    if (logString != null)
                    {
                        // Write the formatted string of the current split log into the log file
                        fileOutput.write(logString.getString().getBytes());
                    }

                    // Is the split log the last log?
                    if (i != indexLastChar)
                    {
                        // Write "\r\n" into the log file
                        fileOutput.write(LogFileConstants.NEW_LINE_CHARACTORS.getBytes());
                    }
                }
            }
            catch (IOException exception)
            {
                // Write the exception into a file
                mLogFileError.write(null, exception);
            }
            finally
            {
                // Apply to the coding standard
            }
        }
    }

    /**
     * Write the end tag into log file.
     * 
     * @param fileOutput The RandomAccessFile object that is used to access the log file.
     *       Range: Valid RandomAccessFile object
     *       Unit: RandomAccessFile
     *       Scaling: 1
     *       
     * @param fileFormat The LogFileFormat object for the end tag.
     *       Range: Valid LogFileFormat object
     *       Unit: LogFileFormat
     *       Scaling: 1
     *       
     * @param endString The SafetyString object for the end string.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @param newLine The SafetyString object for the new line.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @return None
     */
    private void writeLogEnd(final RandomAccessFile fileOutput, final LogFileFormat fileFormat,
            final SafetyString endString, final SafetyString newLine)
    {
        // Get the formatted end string
        final SafetyString logString = fileFormat.getEndwithFormat(endString, newLine);

        // Is the formatted end string equal to null?
        if (logString != null)
        {
            try
            {
                // Write the formatted end string into the log file
                fileOutput.write(logString.getString().getBytes());
            }
            catch (IOException exception)
            {
                // Write the exception into a file
                mLogFileError.write(null, exception);
            }
            finally
            {
                // Apply to the coding standard
            }
        }
    }

    /**
     * Search the log file on the device.
     * If the log file exists, Set mLogFile to the log file.
     * 
     * @param None
     * 
     * @return The result
     *       Range: Valid SafetyBoolean object
     *       Scaling: 1
     */
    private SafetyBoolean searchFile()
    {
        SafetyBoolean isExistLastFile = SafetyBoolean.FALSE;
        
        // Constructs a new file which at data/nugen/
        final File file = new File(ROOT_DIR + SLASH + LOGFILENAME);
        
        synchronized (mLock)
        {
            final boolean isFileExist = file.exists();
            
            // Does the log file exist on the file system?
            if (!isFileExist)
            {
                // Set mLogFile to null
                mLogFile = null;
                
                // Set the result to SafetyBoolean.FALSE
                isExistLastFile = SafetyBoolean.FALSE;
            }
            else
            {
                // Set mLogFile to a new file instance that its path is sdcard/nugen/LogFile/log
                mLogFile = file;
                
                // Set the result to SafetyBoolean.TRUE
                isExistLastFile = SafetyBoolean.TRUE;
            }
        
            // Return the result
            return isExistLastFile;
        }
    }

    /**
     * Split the log line by line.
     * Different lines are encrypted into different SafetyString instances.
     * 
     * @param log The log content
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     * 
     * @return The result
     *       Range: Valid object
     *       Unit: SafetyString
     *       Scaling: 1
     */
    private SafetyString[] splitLog(final String log)
    {
        final String[] logs = log.split(LogFileConstants.NEW_LINE_CHARACTORS);
        final SafetyString[] splitLog = new SafetyString[logs.length];
        
        try
        {
            for (int i = 0; i < logs.length; i++)
            {
                /*
                 * Generate the safety string that removing white space characters 
                 * from the beginning and end of the string.
                 */
                splitLog[i] = new SafetyString(logs[i].trim(), 
                        CRCTool.generateCRC16(logs[i].trim().getBytes()));
            }
        }
        catch (Exception exception)
        {
            // Write the exception into a file
            mLogFileError.write(null, exception);
        }
        finally
        {
            // Apply to the coding standard
        }

        /*
         * Return the result. If the log is too long, the function uses "\r\n" to
         * split the log.
         */
        return splitLog;
    }
    
    /*
     * Check whether the position of the log content in the log file is correct.
     * 
     * @param position The position in HeadPosition in the log file.
     *       Range: 0 ~ (2 ^ 31)-1
     *       Unit: int
     *       Scaling: 1
     *       
     * @return The result 
     *       Range: Valid SafetyBoolean object
     *       Unit: SafetyBoolean
     *       Scaling: 1
     */
    private SafetyBoolean checkPosition(final int... position)
    {
        SafetyBoolean isCorrectPositionValue = SafetyBoolean.TRUE;
        final byte byteOfIsFalseBoolean = SafetyBoolean.FALSE.getByte();
        byte byteOfIsCorrectPositionValue = BYTE_FIELD_UNUSED;
        
        // Is position equal to null?
        if (position != null)
        {
            // Is the length of position larger tahn 0?
            if (position.length > 0)
            {
                // Check whether position is valid
                isCorrectPositionValue = LogFile
                        .checkHeadPositionRange(position[0]);
                
                byteOfIsCorrectPositionValue = isCorrectPositionValue.getByte();
                
                // Is the position is invalid?
                if (byteOfIsCorrectPositionValue == byteOfIsFalseBoolean)
                {
                    // Set the position is invalid
                    isCorrectPositionValue = SafetyBoolean.FALSE;
                }
            }
        }
        
        /*
         * Return the result. If the positions RC app log and RC device log in the
         * log file is incorrect, the function shall return SafetyBoolean.FALSE.
         * Otherwise, the function shall return SafetyBoolean.TRUE.
         */
        return isCorrectPositionValue;
    }
    
    /**
     * Singleton for LogFileWriter. Create and get LogFileWrite instance.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     * 
     * @return The result
     *       Range: Valid object
     *       Unit: LogFileWriter
     *       Scaling: 1
     */
    static synchronized LogFileWriter getInstance(final Context context)
    {
        // Is mLogFileWriter equal to null?
        if (mLogFileWriter == null)
        {
            // Create a new LogFileWriter object
            mLogFileWriter = new LogFileWriter(context);
        }

        // Return the new LogFileWriter object
        return mLogFileWriter;
    }
    
}
