/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: UserSettingXML
 * Brief: The class is used to access the user settings XML file which contains the user settings.
 *
 * Create Date: 10/12/2015
 * $Revision: 23372 $
 * $Author: WilliyChiang $
 * $Id: UserSettingsXml.java 23372 2015-11-05 07:52:14Z WilliyChiang $
 */

package com.accu_chek.solo_m.rcapp.application.settinginterface;

import android.content.Context;

import com.accu_chek.solo_m.rcapp.application.config.MeterParameter;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel.IUserSettingsModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenUserSettingsConstants.UserSettingsKey;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public final class UserSettingsXml implements IUserSettingsModel
{
    
    /**
     * The file path of the user settings file.
     */
    public static final String FILE_PATH = "/data/nugen/";

    /**
     * The file name of the user settings file.
     */
    public static final String NAME_USER_SETTINGS = "UserSettings.xml";
    
    /**
     * The file name of the backup file of user settings.
     */
    public static final String NAME_USER_SETTINGS_BACKUP = "UserSettings_Backup.xml";

    /**
     * The definition is the tag name of root node for user settings in the XML file.
     */
    private static final String TAG_ROOT = "record";

    /**
     * The definition is the tag name of parameter node of user settings in the XML file.
     */
    private static final String TAG_PARAMETER = "Parameter";

    /**
     * The definition is the tag name of type node of user settings in the XML file.
     */
    private static final String TAG_TYPE = "Type";

    /**
     * The definition is the tag name of value node of user settings in the XML file.
     */
    private static final String TAG_VALUE = "Value";

    /**
     * The definition is the tag name of CRC node of user settings in the XML file.
     */
    private static final String TAG_CRC = "CRC";

    /**
     * The regular expression is used to search the parameter from the XML file.
     */
    private static final String PREFIX_SEARCH = "/data_set/SoloM/" + TAG_ROOT + "["
            + TAG_PARAMETER + " = '%s']";
    
    /**
     * The definition is the buffer size. 
     */
    private static final int BUFFER_SIZE = 1024;

    /**
     * The singleton instance of UserSettingsXml.
     */
    private static volatile UserSettingsXml mInstance = new UserSettingsXml();

    /**
     * The XML document instance which stores the data of the user settings.
     */
    private static Document mDOC = null;

    /**
     * The global variable is the File instance which is used to access the XML 
     * file of user settings.
     */
    private static File mFile = new File(FILE_PATH.concat(NAME_USER_SETTINGS));

    /**
     * The initial function of the UserSettingsXML class.
     * 
     * @param None
     * 
     * @return None
     */
    static
    {
        loadUserSettings();
    }
    
    /**
    /**
     * The constructor of UserSettingXML. To avoid static code analysis issues,
     * the class needs a private constructor.
     * 
     * @param None
     *       
     * @return None
     */
    private UserSettingsXml()
    {
        // Avoid multiple instance
    }

    /**
     * Get the value by using the parameter key to search the user settings XML file.
     * If the key can't be found, the function shall return null.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @param key The name of a setting
     *       Range: Valid UserSettingsKey object
     *       Unit: UserSettingsKey
     *       Scaling: 1
     * 
     * @return The value of the parameter key. If the parameter key can't be found, 
     *         the function shall return null.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     */
    @Override
    public SafetyString getValue(final Context context, final UserSettingsKey key)
    {
        SafetyString result = null;
        MeterParameter parameter = null;

        try
        {
            /* 
             * Search the user settings XML file by using the parameter key in 
             * order to find the record of the parameter key
             */
            final Node record = (Node)XPathFactory.newInstance().newXPath()
                    .evaluate(String.format(PREFIX_SEARCH, key.getUserSettingKey()),
                            mDOC, XPathConstants.NODE);
            
            // Get the child node of record
            final NodeList children = record.getChildNodes();
            
            // Get the length of the child nodes
            final int childNodeLength = children.getLength();

            parameter = new UserSettingsParameter();

            for (int i = 0; i < childNodeLength; i++)
            {
                // Get the index-th child node
                final Node node = children.item(i);
                
                // Get the name of the index-th child node
                final String nodeName = node.getNodeName();

                // Is the name of the index-th child node equal to "Parameter"?
                if (nodeName.equalsIgnoreCase(TAG_PARAMETER))
                {
                    /*
                     *  Set the ID name of the UserSettingsParameter instance 
                     *  to the text of the index-th child node
                     */
                    parameter.setIDName(node.getTextContent());
                }
                // Is the name of the index-th child node equal to "Type"?
                else if (nodeName.equalsIgnoreCase(TAG_TYPE))
                {
                    /*
                     *  Set the type of the UserSettingsParameter instance 
                     *  to the text of the index-th child node
                     */
                    parameter.setType(node.getTextContent());
                }
                // Is the name of the index-th child node equal to "Value"?
                else if (nodeName.equalsIgnoreCase(TAG_VALUE))
                {
                    /*
                     *  Set the value of the UserSettingsParameter instance 
                     *  to the text of the index-th child node
                     */
                    parameter.setValue(node.getTextContent());
                }
                // Is the name of the index-th child node equal to "Parameter"?
                else if (nodeName.equalsIgnoreCase(TAG_CRC))
                {
                    /*
                     *  Set the CRC of the UserSettingsParameter instance 
                     *  to the text of the index-th child node
                     */
                    parameter.setCRC(node.getTextContent());
                }
                else
                {
                    // Apply to coding standard
                }
            }
        }
        catch (XPathExpressionException exception)
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
        finally
        {
            // Apply to coding standard
        }

        // Is the UserSettingsParameter instance equal to null?
        if (null != parameter)
        {
            // Get the value of the UserSettingsParameter instance
            final String value = parameter.getValue();

            // Transfer the value with string type too the value with SafetyString type
            result = new SafetyString(value, CRCTool.generateCRC16(value
                    .getBytes()));
        }

        // Return the result
        return result;
    }

    /**
     * Search the node by input key, replace the value and CRC by input value.
     * After that, refresh the user settings XML file.
     * 
     * @param context The context of the caller
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *            
     * @param key The name of a setting
     *       Range: Valid UserSettingsKey object
     *       Unit: UserSettingsKey
     *       Scaling: 1
     *            
     * @param value The new value of a setting
     *       Range: Valid SafetyString object
     *       Unit: SafetyString.
     *       Scaling: 1
     *       
     * @return None 
     */
    @Override
    public void setValue(final Context context, final UserSettingsKey key,
            final SafetyString value)
    {
        try
        {
            Node nodeParameter = null;
            Node nodeType = null;
            Node nodeValue = null;
            Node nodeCrc = null;
            
            /* 
             * Search the user settings XML file by using the parameter key in 
             * order to find the record of the parameter key
             */
            final Node record = (Node)XPathFactory.newInstance().newXPath()
                    .evaluate(String.format(PREFIX_SEARCH, key.getUserSettingKey()),
                            mDOC, XPathConstants.NODE);
            
            // Get the child node of record
            final NodeList children = record.getChildNodes();
            
            // Get the length of the child nodes
            final int childNodeLength = children.getLength();

            for (int i = 0; i < childNodeLength; i++)
            {
                // Get the index-th child node
                final Node node = children.item(i);
                
                // Get the name of the index-th child node
                final String nodeName = node.getNodeName();

                // Is the name of the index-th child node equal to "Parameter"?
                if (nodeName.equalsIgnoreCase(TAG_PARAMETER))
                {
                    // Set a new Node instance to the child node 
                    nodeParameter = node;
                }
                // Is the name of the index-th child node equal to "Type"?
                else if (nodeName.equalsIgnoreCase(TAG_TYPE))
                {
                    // Set a new Node instance to the child node 
                    nodeType = node;
                }
                // Is the name of the index-th child node equal to "Value"?
                else if (nodeName.equalsIgnoreCase(TAG_VALUE))
                {
                    // Set a new Node instance to the child node 
                    nodeValue = node;
                }
                // Is the name of the index-th child node equal to "CRC"?
                else if (nodeName.equalsIgnoreCase(TAG_CRC))
                {
                    // Set a new Node instance to the child node 
                    nodeCrc = node;
                }
                else
                {
                    // Apply to coding standard
                }
            }

            // Store the value of the key to the XML file of user settings
            storeSettingToFile(nodeParameter, nodeType, nodeValue, nodeCrc, value);
        }
        catch (XPathExpressionException exception)
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
        finally
        {
            // Apply to coding standard
        }
    }
    
    /**
     * Reset the values of user settings to default values.
     * 
     * @param None
     *       
     * @return None 
     */
    public void setValueToDefault()
    {
        boolean isSuccess = false;
        
        // Delete the XML file of user settings
        isSuccess = mFile.delete();
        
        // Is the XML file of user settings deleted?
        if (false == isSuccess)
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);        
        }
        
        try
        {
            final byte[] buffer = new byte[BUFFER_SIZE];
            int lengthe = 0;
            final File backupFile = new File(FILE_PATH.concat(NAME_USER_SETTINGS_BACKUP));
            final InputStream inputStream = new FileInputStream(backupFile);
            final OutputStream outputStream = new FileOutputStream(mFile);

            // Read the backup XML file of user settings to the buffer
            lengthe = inputStream.read(buffer);
            
            // Are there some bytes which are needed to copy? 
            while (lengthe > 0)
            {
                // Write to the XML file of user settings from buffer
                outputStream.write(buffer, 0, lengthe);
                
                // Read the backup file to the buffer
                lengthe = inputStream.read(buffer);
            }
            
            // Close the InputStream instance
            inputStream.close();
            
            // Close the outStream instance
            outputStream.close();
        }
        catch (FileNotFoundException exception)
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
        catch (IOException exception)
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
        finally
        {
            // Apply to the coding standard
        }
        
    }
    
    /**
     * Store the value of a setting in the XML file of user settings.
     * 
     * @param nodeParameter The data of the parameter node.
     *       Range: Valid Node object
     *       Unit: Node
     *       Scaling: 1
     *            
     * @param nodeType The data of the type node
     *       Range: Valid Node object
     *       Unit: Node
     *       Scaling: 1
     *            
     * @param nodeValue The data of the value node
     *       Range: Valid Node object
     *       Unit: Node
     *       Scaling: 1
     *       
     * @param nodeCrc The data of the CRC node
     *       Range: Valid Node object
     *       Unit: Node
     *       Scaling: 1
     *       
     * @param value The new value of a setting
     *       Range: Valid SafetyString object
     *       Unit: SafetyString.
     *       Scaling: 1
     *       
     * @return None 
     */
    private void storeSettingToFile(final Node nodeParameter, final Node nodeType, 
            final Node nodeValue, final Node nodeCrc, final SafetyString value)
    {
        // Are the parameters valid?
        if ((nodeParameter != null) && (nodeType != null) && (nodeCrc != null))
        {
            // Set the Node instance which contains the value to the string of value
            nodeValue.setTextContent(value.getString());
            
            try
            {
                /*
                 *  Generate the new CRC, and Set the Node instance which contains 
                 *  the CRC to the string of value
                 */
                nodeCrc.setTextContent(String.valueOf(CRCTool
                        .generateCRC16(nodeParameter.getTextContent()
                                .concat(nodeType.getTextContent())
                                .concat(nodeValue.getTextContent()).getBytes())));
    
                // Transform mDOC to the user settings XML file
                TransformerFactory.newInstance().newTransformer()
                        .transform(new DOMSource(mDOC), new StreamResult(mFile));
            }
            catch (TransformerException exception)
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);
            }
            catch (TransformerFactoryConfigurationError exception)
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);
            }
            finally
            {
                // Apply to coding standard
            }
        }
        else
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46301);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
    }
    
    /**
     * Parse the user settings XML file and store the result in a document instance.
     * 
     * @param None
     * 
     * @return None
     */
    public static void loadUserSettings()
    {
        try
        {
            // Parse the user settings XML file, and set mDoc to the result 
            mDOC = DocumentBuilderFactory.newInstance().newDocumentBuilder()
                    .parse(mFile);
        }
        catch (SAXException exception)
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
        catch (IOException exception)
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
        catch (ParserConfigurationException exception)
        {
            final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
            
            // Show EMWR event
            NotifyProxy.showEMWR(emwrMessage);
        }
        finally
        {
            // Apply to coding standard
        }
    }

    /**
     * Get the singleton instance of the UserSettingsXML class.
     * 
     * @param None
     * 
     * @return The singleton UserSettingsXml instance
     *       Range: Valid UserSettingsXml object
     *       Unit: UserSettingsXml
     *       Scaling: 1
     */
    public static UserSettingsXml getInstance()
    {
        // Return the singleton instance of the UserSettingsXML class
        return mInstance;
    }

    /*
     * This class is used to provide the functionality to check the integrity for
     * a value of user setting.
     */
    private static class UserSettingsParameter extends MeterParameter
    {
        
        /**
        * Check the data integrity via CRC check before return the value.
        * 
         * @param None
         *            
         * @return The value of a setting
         *       Range: Valid String object
         *       Unit: String.
         *       Scaling: 1
        */
        @Override
        public String getValue()
        {
            final byte byteOfSafetyBooleanFalse = SafetyBoolean.FALSE.getByte();
            
            /*
             * Compare the stored CRC and the CRC which is calculated by 
             * mStrIDName, mStrType and mStrValue
             */
            final byte byteOfIsCRCCorrect = compareCRC().getByte();
            
            // Get the value of the setting
            String result = null;

            // Is the result of CRC comparison failed?
            if (byteOfSafetyBooleanFalse == byteOfIsCRCCorrect)
            {
                final NotifyMessage emwrMessage = new NotifyMessage(EMWRList.EMW46302);
                
                // Show EMWR event
                NotifyProxy.showEMWR(emwrMessage);
            }
            else
            {
                result = super.getValue();
            }

            // Return the result
            return result;
        }
        
    }
    
}

/*
 * ===========================================================================
 * 
 * Revision history
 * 
 * ===========================================================================
 */