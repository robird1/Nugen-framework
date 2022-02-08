/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: MeterParameterMatrixWriteBody
 * Brief: The class is used to access the user settings XML file which contains the user settings.
 *
 * Create Date: 10/12/2015
 * $Revision: $
 * $Author: $
 * $Id: $
 */

package com.accu_chek.solo_m.rcapp.application.config;

import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyNumber;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

import java.io.File;
import java.io.IOException;

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

public final class MeterParameterMatrixWriteBody
{
    
    /**
     * The file path of the user settings file.
     */
    public static final String FILE_PATH = "/data/ConfigMatrix/";

    /**
     * The file name of the Configuration Matrix.
     */
    public static final String NAME_CONFIGURATION_MATRIX = "CM_MeterParameter.xml";
    
    /**
     * The file name of the user settings file.
     */
    public static final String NAME_BACKUP_CONFIGURATION_MATRIX = "CM_MeterParameter_Backup.xml";

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
    private static final String PREFIX_SEARCH = "/data_set/" + TAG_ROOT + "["
            + TAG_PARAMETER + " = '%s']";

    /**
     * The singleton instance of UserSettingsXml.
     */
    private static volatile MeterParameterMatrixWriteBody mInstance = new MeterParameterMatrixWriteBody();

    /**
     * The XML document instance which stores the data of the Configuration Matrix.
     */
    private static Document mDOC = null;
    
    /**
     * The XML document instance which stores the data of the backup Configuration Matrix.
     */
    private static Document mBackupDOC = null;

    /**
     * The global variable is the File instance which is used to access the Configuration Matrix.
     */
    private static File mFile = new File(FILE_PATH.concat(NAME_CONFIGURATION_MATRIX));
    
    /**
     * The global variable is the File instance which is used to access the backup Configuration Matrix.
     */
    private static File mBackupFile = new File(FILE_PATH.concat(NAME_BACKUP_CONFIGURATION_MATRIX));

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
    private MeterParameterMatrixWriteBody()
    {
        // Avoid multiple instance
    }

    /**
     * Set the SafetyString value of a setting to the Configuration Matrix.
     * 
     * @param ssIDName The key of a setting.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @param value The value of a setting.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @return None
     */   
    public final void setString(final SafetyString ssIDName, final SafetyString value)
    {
        // Is the parameters valid?
        if ((ssIDName != null) && (value != null))
        {
            // Set value of key to the Configuration Matrix
            setValue(ssIDName, value);
            
            // Set value of key to the backup Configuration Matrix
            setBackupValue(ssIDName, value);
        }
        else
        {
            callEmwr();
        }
    }
    
    /**
     * Set the SafetyNumber<Long> value of a setting to the Configuration Matrix.
     *       
     * @param ssIDName The key of a setting.
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *       
     * @param value The value of a setting.
     *       Range: Valid SafetyNumber<Long> object
     *       Unit: SafetyNumber<Long>
     *       Scaling: 1
     *       
     * @return None
     */   
    public final void setLong(final SafetyString ssIDName, final SafetyNumber<Long> value)
    {
        // Is the parameters valid?
        if ((ssIDName != null) && (value != null))
        {
            String longValue = null;
            int crc = 0;
        
            // Transfer value with SafetyNumber<Long> type to value with string type
            longValue = String.valueOf(value.get());
            
            // Generate the CRC of value with string type 
            crc = CRCTool.generateCRC16(longValue.getBytes());
            
            // Set value with SafetyString type of key to the Configuration Matrix
            setValue(ssIDName, new SafetyString(longValue, crc));
            
            // Set value with SafetyString type of key to the backup Configuration Matrix
            setBackupValue(ssIDName, new SafetyString(longValue, crc));
        }
        else
        {
            callEmwr();
        }
    }
    
    /**
     * Set the SafetyNumber<Integer> value of a setting to a model.
     * 
     * @param context The context of the caller.
     *       Range: Valid Context object
     *       Unit: Context
     *       Scaling: 1
     *       
     * @param model The place where stores data.
     *       Range: Valid IUserSettingsModel object
     *       Unit: IUserSettingsModel
     *       Scaling: 1 
     *       
     * @param key The key of a setting.
     *       Range: Valid UserSettingsKey object
     *       Unit: UserSettingsKey
     *       Scaling: 1
     *       
     * @param value The value of a setting.
     *       Range: Valid SafetyNumber<Integer> object
     *       Unit: SafetyNumber<Integer>
     *       Scaling: 1
     *       
     * @return None
     */   
    public final void setInteger(final SafetyString ssIDName, final SafetyNumber<Integer> value)
    {
        // Is the parameters valid?
        if ((ssIDName != null) && (value != null))
        {
            // Transfer value with SafetyNumber<Integer> type to value with string type
            final String intValue = String.valueOf(value.get());
            
            // Generate the CRC of value with string type
            final int crc = CRCTool.generateCRC16(intValue.getBytes());
            
            // Set the value with SafetyString type of key to the Configuration Matrix
            setValue(ssIDName, new SafetyString(intValue, crc));
            
            // Set the value with SafetyString type of key to the backup Configuration Matrix
            setBackupValue(ssIDName, new SafetyString(intValue, crc));    
        }
        else
        {
            callEmwr();
        }
    }
    
    /**
     * Search the node by ssIDName, replace the value and CRC by input value.
     * After that, refresh the Configuration Matrix file.
     *            
     * @param ssIDName The name of a setting
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *            
     * @param value The new value of a setting
     *       Range: Valid SafetyString object
     *       Unit: SafetyString.
     *       Scaling: 1
     *       
     * @return None 
     */
    private void setValue(final SafetyString ssIDName, final SafetyString value)
    {
        Debug.printI("williy", "setValue is entered");
        
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
                    .evaluate(String.format(PREFIX_SEARCH, ssIDName.getString()),
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
            callEmwr();
        }
        finally
        {
            // Apply to coding standard
        }
        
        Debug.printI("williy", "setValue is existed");
    }
    
    /**
     * Search the node by ssIDName, replace the value and CRC by input value.
     * After that, refresh the Configuration Matrix file.
     *            
     * @param ssIDName The name of a setting
     *       Range: Valid SafetyString object
     *       Unit: SafetyString
     *       Scaling: 1
     *            
     * @param value The new value of a setting
     *       Range: Valid SafetyString object
     *       Unit: SafetyString.
     *       Scaling: 1
     *       
     * @return None 
     */
    private void setBackupValue(final SafetyString ssIDName, final SafetyString value)
    {
        Debug.printI("williy", "setBackupValue is entered");
        
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
                    .evaluate(String.format(PREFIX_SEARCH, ssIDName.getString()),
                            mBackupDOC, XPathConstants.NODE);
            
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
            storeSettingToBackupFile(nodeParameter, nodeType, nodeValue, nodeCrc, value);
        }
        catch (XPathExpressionException exception)
        {
            callEmwr();
        }
        finally
        {
            // Apply to coding standard
        }
        
        Debug.printI("williy", "setBackupValue is existed");
    }
    
    /**
     * Store the value of a setting in the Configuration Matrix.
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
                callEmwr();
            }
            catch (TransformerFactoryConfigurationError exception)
            {
                callEmwr();
            }
            finally
            {
                // Apply to coding standard
            }
        }
        else
        {
            callEmwr();
        }
    }
    
    /**
     * Store the value of a setting in the backup Configuration Matrix.
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
    private void storeSettingToBackupFile(final Node nodeParameter, final Node nodeType, 
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
                        .transform(new DOMSource(mBackupDOC), new StreamResult(mBackupFile));
            }
            catch (TransformerException exception)
            {
                callEmwr();
            }
            catch (TransformerFactoryConfigurationError exception)
            {
                callEmwr();
            }
            finally
            {
                // Apply to coding standard
            }
        }
        else
        {
            callEmwr();
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
            // Parse the Configuration Matrix, and set mDoc to the result 
            mDOC = DocumentBuilderFactory.newInstance().newDocumentBuilder()
                    .parse(mFile);
            
            // Parse the backup Configuration Matrix, and set mBackupDOC to the result 
            mBackupDOC = DocumentBuilderFactory.newInstance().newDocumentBuilder()
                    .parse(mBackupFile);
        }
        catch (SAXException exception)
        {
            callEmwr();
        }
        catch (IOException exception)
        {
            callEmwr();
        }
        catch (ParserConfigurationException exception)
        {
            callEmwr();
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
    public static MeterParameterMatrixWriteBody getInstance()
    {
        // Return the singleton instance of the UserSettingsXML class
        return mInstance;
    }
    
    /**
     * Trigger EMWR for error handling
     * 
     * @param None
     * 
     * @return None
     */
    private static void callEmwr()
    {
        // TODO : Fix it when EMWR and Log is ready
        //throw new Exception("Error happens!! Switch to Safe State");
    }
    
}
