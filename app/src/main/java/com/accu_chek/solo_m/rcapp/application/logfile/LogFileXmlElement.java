/**
 * ===========================================================================
 * Copyright 2015 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: LogFileXmlElement
 * Brief: The class is used to translate log to XML format
 *
 * Create Date: 07/15/2015
 * $Revision: 19842 $
 * $Author: AdamChen $
 * $Id: LogFileXmlElement.java 19842 2015-09-25 09:33:00Z AdamChen $
 */

package com.accu_chek.solo_m.rcapp.application.logfile;

import android.util.Xml;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import org.xmlpull.v1.XmlSerializer;

public class LogFileXmlElement
{
    
    /**
     * The encoding of XmlSerializer
     */
    private static final String XML_SERIALIZER_ENCODING = "ISO-8859-1";
    
    /**
     * The XmlSerializer instance of LogFileXMLElement.
     */
    private XmlSerializer mXmlSerializer = null;

    /**
     * The ByteArrayOutputStream instance of LogFileXMLElement.
     */
    private ByteArrayOutputStream mWriter = null;
    
    /**
     * The global variable is used to log the errors of the LogFileXmlElement class.
     */
    private LogLogFileError mLogFileError = new LogLogFileError();

    /**
     * The constructor of LogFileXMLElement. Initialized the global variables.
     * 
     * @param None
     * 
     * @return None
     */    
    public LogFileXmlElement()
    {
        // Set mXmlSerializer to a new serializer
        mXmlSerializer = Xml.newSerializer();
        
        // Set mWriter to a new ByteArrayOutputStream
        mWriter = new ByteArrayOutputStream();

        try
        {
            // Set mXmlSerializer to use binary output stream with ISO-8859-1 encoding
            mXmlSerializer.setOutput(mWriter, XML_SERIALIZER_ENCODING);
        }
        // Does illegal argument exception occur?
        catch (IllegalArgumentException exception)
        {
            // Write the exception into a file
            mLogFileError.write(null, exception);
        }
        // Does illegal state exception occur?
        catch (IllegalStateException exception)
        {
            // Write the exception into a file
            mLogFileError.write(null, exception);
        }
        // Does IO exception occur?
        catch (IOException exception)
        {
            // Write the exception into a file
            mLogFileError.write(null, exception);
        }
        // Does null pointer exception occur?
        catch (NullPointerException exception)
        {
            // Write the exception into a file
            mLogFileError.write(null, exception);
        }
        finally
        {
            // Apply to the coding standard
        }
    }

    /**
     * Add new tag into the XmlSerializer object that is created by this instance.
     * 
     * @param tagName The tag name of a log.
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     * 
     * @return None
     */
    public final void newTag(final String tagName)
    {
        try
        {
            // Write a start tag with the tagName
            mXmlSerializer.startTag(null, tagName);
        }
        // Does illegal argument exception occur?
        catch (IllegalArgumentException exception)
        {
            // Write the exception into a file
            mLogFileError.write(tagName, exception);
        }
        // Does illegal state exception occur?
        catch (IllegalStateException exception)
        {
            // Write the exception into a file
            mLogFileError.write(tagName, exception);
        }
        // Does IO exception occur?
        catch (IOException exception)
        {
            // Write the exception into a file
            mLogFileError.write(tagName, exception);
        }
        // Does null pointer exception occur?
        catch (NullPointerException exception)
        {
            // Write the exception into a file
            mLogFileError.write(tagName, exception);
        }
        finally
        {
            // Apply to the coding standard
        }
    }

    /**
     * Add new attribute of recent tag into the XmlSerializer object that is created by this instance.
     * 
     * @param attributeName The attribute name of a log.
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @param attributeValue The attribute value of a log.
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @return None
     */
    public final void addTagAttribute(final String attributeName, final String attributeValue)
    {
        try
        {
            // Write an attribute attrName with attributeValue
            mXmlSerializer.attribute(null, attributeName, attributeValue);
        }
        // Does illegal argument exception occur?
        catch (IllegalArgumentException exception)
        {
            // Write the exception into a file
            mLogFileError.write(attributeName, exception);
        }
        // Does illegal state exception occur?
        catch (IllegalStateException exception)
        {
            // Write the exception into a file
            mLogFileError.write(attributeName, exception);
        }
        // Does IO exception occur?
        catch (IOException exception)
        {
            // Write the exception into a file
            mLogFileError.write(attributeName, exception);
        }
        // Does null pointer exception occur?
        catch (NullPointerException exception)
        {
            // Write the exception into a file
            mLogFileError.write(attributeName, exception);
        }
        finally
        {
            // Apply to the coding standard
        }
    }

    /**
     * Add new attribute of recent tag into the XmlSerializer object
     * that is created by this instance.
     * 
     * @param attrName The attribute name of a log.
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     *       
     * @param attributeValue The attribute value of a log.
     *       Range: -2^31 - (2^31)-1
     *       Unit: int
     *       Scaling: 1
     *       
     * @return None
     */
    public final void addTagAttribute(final String attributeName, final int attributeValue)
    {
        try
        {
            // Write an attribute attrName with attributeValue
            mXmlSerializer.attribute(null, attributeName, String.valueOf(attributeValue));
        }
        // Does illegal argument exception occur?
        catch (IllegalArgumentException exception)
        {
            // Write the exception into a file
            mLogFileError.write(attributeName, exception);
        }
        // Does illegal state exception occur?
        catch (IllegalStateException exception)
        {
            // Write the exception into a file
            mLogFileError.write(attributeName, exception);
        }
        // Does IO exception occur?
        catch (IOException exception)
        {
            // Write the exception into a file
            mLogFileError.write(attributeName, exception);
        }
        // Does null pointer exception occur?
        catch (NullPointerException exception)
        {
            // Write the exception into a file
            mLogFileError.write(attributeName, exception);
        }
        finally
        {
            // Apply to the coding standard
        }
    }

    /**
     * End recent tag into the XmlSerializer object that is created by this instance.
     * 
     * @param tagName The tag name of a log.
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     * 
     * @return None
     */
    public final void endTag(final String tagName)
    {
        try
        {
            // Write end tag
            mXmlSerializer.endTag(null, tagName);
            
            // Write all pending output to the stream
            mXmlSerializer.flush();
        }
        // Does illegal argument exception occur?
        catch (IllegalArgumentException exception)
        {
            // Write the exception into a file
            mLogFileError.write(tagName, exception);
        }
        // Does illegal state exception occur?
        catch (IllegalStateException exception)
        {
            // Write the exception into a file
            mLogFileError.write(tagName, exception);
        }
        // Does IO exception occur?
        catch (IOException exception)
        {
            // Write the exception into a file
            mLogFileError.write(tagName, exception);
        }
        // Does null pointer exception occur?
        catch (NullPointerException exception)
        {
            // Write the exception into a file
            mLogFileError.write(tagName, exception);
        }
        finally
        {
            // Apply to the coding standard
        }
    }

    /**
     * Get XML content in String format.
     * 
     * @param None
     * 
     * @return String
     *       Range: Null, valid String object
     *       Unit: String
     *       Scaling: 1
     */
    final String getString()
    {
        String returnString = null;

        try
        {
            // Write all pending output to the stream
            mXmlSerializer.flush();

            // Get mWriter
            returnString = mWriter.toString();
        }
        // Does IO exception occur?
        catch (IOException exception)
        {
            // Write the exception into a file
            mLogFileError.write(null, exception);
        }
        // Does null pointer exception occur?
        catch (NullPointerException exception)
        {
            // Write the exception into a file
            mLogFileError.write(null, exception);
        }
        finally
        {
            // Apply to the coding standard
        }

        // Return mWriter 
        return returnString;
    }
    
}
