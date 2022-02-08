package com.accu_chek.solo_m.rcapp.application.continua;

import java.lang.ref.WeakReference;

import android.content.Context;
import android.util.Log;

/**
 * This class used to load the system Continua library and establish the connection
 * to the library. This class provides some function for setup Continua Agent,
 * send and receive the data with it. 
 */
public final class MindtreeService 
{
    /**
     * Load the system Continua library.
     */
    static 
    {
        System.load("/system/lib/libmindtree_jni.so");
    }    
    
    /**
     * The interface for receiving the request command from Continua Agent. 
     */
    public interface OnJNIListener 
    {
        /**
         * Call back function of receiving command from Continua Agent.
         *
         * @param requestType : The request command from Continua manager.
         *        Range: Refer to the definition of ContinuaRequestType.
         *        Unit: Integer.
         *        Scaling: 1.
         * @param configId : It used to recognize which type of configuration
         * should be used.
         *        Range: Refer to the definition of GlucoseConfigId.
         *        Unit: Integer.
         *        Scaling: 1.
         * @param segmentId : The segmentId of the configuration.
         *        Range: Refer to the definition of GlucoseSegmentId.
         *        Unit: Integer.
         *        Scaling: 1.
         *   
         * return void [out]: None
         */
        void onRequestReceived(int requestType, int configId, int segmentId);   
        
        /**
         * Call back function of receiving command with data from Continua Agent.
         *
         * @param requestType : The request command from Continua manager.
         *        Range: Refer to the definition of ContinuaRequestType.
         *        Unit: Integer.
         *        Scaling: 1.
         * @param configId : It used to recognize which type of configuration
         * should be used.
         *        Range: Refer to the definition of GlucoseConfigId.
         *        Unit: Integer.
         *        Scaling: 1.
         * @param segmentId : The segmentId of the configuration.
         *        Range: Refer to the definition of GlucoseSegmentId.
         *        Unit: Integer.
         *        Scaling: 1.
         * @param data : The data which contains the content of request.
         *        Range: Valid object of byte[].
         *        Unit: byte[].
         *        Scaling: 1.
         *        
         * return void [out]: None
         */
        void onRequestWithDataReceived(int requestType, int configId, int segmentId, byte[] data);
        
        /**
         * Call back function of receiving RPC command from Continua Agent.
         *
         * @param data : The data which contains the content of request.
         *        Range: Valid object of byte[].
         *        Unit: byte[].
         *        Scaling: 1.
         *        
         * return void [out]: None
         */
        void onRPCCommandReceived(byte[] data);
        
        /**
         * Call back function of receiving get RPC key command from Continua Agent.
         *
         * @param keyType : The value of key type.
         *        Range: Refer to the definition of AuthenticationKey.
         *        Unit: Integer.
         *        Scaling: 1.
         * 
         * return byte[] [out]: The data which contains the authentication key.
         *        Range: Valid object of byte[].
         *        Unit: byte[].
         *        Scaling: 1.
         */
        byte[] getRPCKey(int keyType);
        
        /**
         * Call back function of receiving the data event report of write file command.
         * 
         * @param data : The data contains the file in ADPU size.
         * 		  Range: Valid object of byte[].
         * 		  Unit: byte[].
         * 	  	  Scaling: 1.
         * 
         * return void [out]: None.
         */
        void onDataEventReport(byte[] data);
    }

    /**
     * The callback function which will be called when command coming.
     */
    private static OnJNIListener mListener = null;
    
    /**
     * Accessed by native methods.
     */
    private int mNativeContext = -1;
   
    /**
     * Assign the callback function and setup Continua Agent.
     * 
     * @param context : The application context. Context is provided by Android.
     *        Range: Valid Context object in Android.
     *        Unit: Context.
     *        Scaling: 1.
     * @param listener : The callback for receiving commands from Continua Agent.
     *        Range: Valid object of OnJNIListener.
     *        Unit: OnJNIListener.
     *        Scaling: 1.
     *        
     * see mListener [out]
     */
    public MindtreeService(Context context, OnJNIListener listener) 
    {
       mListener = listener;
       
       _native_setup(new WeakReference<MindtreeService>(this));
    }

    /**
     * Initiate the Continua Agent module.
     *
     * return void [out]: None
     */
    public void initContinua() 
    {
       _initContinua();    
    }
    
    /**
     * Stop the Continua Agent module.
     *
     * return void [out]: None
     */
    public void closeContinua() 
    {  
       _CloseContinua();
    }          
    
    /**
     * Delegate the segment data to Continua Agent.
     *
     * @param type : The command of this segment data.
     *        Range: Valid object of ContinuaCommand.
     *        Unit: ContinuaCommand.
     *        Scaling: 1. 
     * @param value : The target segment data.
     *        Range: Valid object of each segment data definition.
     *        Unit: byte[].
     *        Scaling: 1.
     *        
     * return void [out]: None
     */
    public void setIEEEObjectData(int type, byte[] value) 
    {  
       _SetIEEEObjectData(type, value);
    }
    
    /**
     * Delegate the 10417 segment data to Continua Agent.
     *
     * @param type : The command of this segment data.
     *        Range: Valid object of ContinuaCommand.
     *        Unit: ContinuaCommand.
     *        Scaling: 1. 
     * @param value : The target segment data.
     *        Range: Valid object of each segment data definition.
     *        Unit: byte[].
     *        Scaling: 1.
     *        
     * return void [out]: None
     */
    public void setContinuaDataOf10417(int type, byte[] value)
    {
        _SetContinuaDataOf10417(type, value);
    }
    
    /**
     * Delegate the 10419 segment data to Continua Agent.
     *
     * @param type : The command of this segment data.
     *        Range: Valid object of ContinuaCommand.
     *        Unit: ContinuaCommand.
     *        Scaling: 1. 
     * @param value : The target segment data.
     *        Range: Valid object of each segment data definition.
     *        Unit: byte[].
     *        Scaling: 1.
     *        
     * return void [out]: None
     */
    public void setContinuaDataOf10419(int type, byte[] value)
    {
        _SetContinuaDataOf10419(type, value);
    }
    
    /**
     * Delegate the RPC data to Continua Agent.
     *
     * @param value : The target segment data.
     *        Range: Valid object of each segment data definition.
     *        Unit: byte[].
     *        Scaling: 1.
     *        
     * return void [out]: None
     */
    public void setContinuaDataOfRPC(byte[] value)
    {
        _SetContinuaDataOfRPC(value);
    }

    /**
     * Release the Continua Agent instance.
     *
     * return void [out]: None
     */
    public void release() 
    {  
       _native_release();
    }
    
    /**
     * Callback function of receiving command from Continua Agent.
     *
     * @param requestType : The request command from Continua manager.
     *        Range: Refer to the definition of ContinuaRequestType.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param configId : It is used to recognize which type of configuration
     * should be used.
     *        Range: Refer to the definition of GlucoseConfigId.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param segmentId : The segmentId of the configuration.
     *        Range: Refer to the definition of GlucoseSegmentId.
     *        Unit: Integer.
     *        Scaling: 1.
     * 
     * return void [out]: None
     */
    private static void postEventFromNative(int requestType, int configId, 
            int segmentId)
    {
        Log.i("MindtreeService", "requestType = " + requestType + " configId = " 
                + configId + " segmentId = "+ segmentId);
        
        mListener.onRequestReceived(requestType, configId, segmentId);
    }
    
    /**
     * Callback function of receiving command with data from Continua Agent.
     *
     * @param requestType : The request command from Continua manager.
     *        Range: Refer to the definition of ContinuaRequestType.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param configId : It is used to recognize which type of configuration
     * should be used.
     *        Range: Refer to the definition of GlucoseConfigId.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param segmentId : The segmentId of the configuration.
     *        Range: Refer to the definition of GlucoseSegmentId.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param data : The data which contains the content of request.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.
     * 
     * return void [out]: None
     */
    private static void postEventWithData(int requestType, int configId, 
            int segmentId, byte[] data)
    {
        Log.i("MindtreeService", "requestType = " + requestType + " configId = " 
                + configId + " segmentId = "+ segmentId);
        
        mListener.onRequestReceived(requestType, configId, segmentId);
    }
    
    /**
     * Callback function of receiving RPC command from Continua Agent.
     *
     * @param data : The data which contains the content of request.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.
     * 
     * return void [out]: None
     */
    private static void postEventWithRPCCommand(byte[] data)
    {
        Log.i("MindtreeService", "length = " + data.length);
        
        mListener.onRPCCommandReceived(data);
    }
    
    /**
     * Return the RPC authentication key according to the input type.
     *
     * @param keyType : The value of key type.
     *        Range: Refer to the definition of AuthenticationKey.
     *        Unit: Integer.
     *        Scaling: 1.
     * 
     * return byte[] [out]: The data which contains the authentication key.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.
     */
    private static byte[] getRPCKey(int keyType)
    {        
        return mListener.getRPCKey(keyType);
    }
    
    /**
     * Delegate the data event report data via mListener.
     * 
     * @param data : The data contains the file in ADPU size.
     * 		  Range: Valid object of byte[].
     * 		  Unit: byte[].
     * 	  	  Scaling: 1.
     * 
     * return void [out]: None.
     */
    private static void postDataEventReport(byte[] data)
    {
    	mListener.onDataEventReport(data);
    }
    
    /**
     * Setup the Continua Agent and put the instance to JNI.
     *
     * @param mindtree_this : The instance of this object.
     *        Range: Valid object of MindtreeService.
     *        Unit: MindtreeService.
     *        Scaling: 1.
     *        
     * return void [out]: None.
     */
    private native final void _native_setup(Object mindtree_this);
    
    /**
     * Release the Continua Agent instance.
     *
     * return void [out]: None.
     */
    private native final void _native_release();
    
    /**
     * Initial the Continua Agent for listening to the commands.
     *
     * return void [out]: None.
     */
    private native final void _initContinua();
    
    /**
     * Close the Continua Agent.
     *
     * return void [out]: None.
     */
    private native final void _CloseContinua();
    
    /**
     * Delegate the required data to Continua Agent.
     * 
     * @param type : The command of this segment data.
     *        Range: Valid object of ContinuaCommand.
     *        Unit: ContinuaCommand.
     *        Scaling: 1.
     * @param value : The target segment data.
     *        Range: Valid object of each segment data definition.
     *        Unit: SafetyByteArray.
     *        Scaling: 1.
     * 
     * return void [out]: None.
     */
    private native final void _SetIEEEObjectData(int type, byte[] value);
    
    /**
     * Delegate the required 10417 data to Continua Agent.
     * 
     * @param type : The command of this segment data.
     *        Range: Valid object of ContinuaCommand.
     *        Unit: ContinuaCommand.
     *        Scaling: 1.
     * @param value : The target segment data.
     *        Range: Valid object of each segment data definition.
     *        Unit: SafetyByteArray.
     *        Scaling: 1.
     * 
     * return void [out]: None.
     */
    private native final void _SetContinuaDataOf10417(int type, byte[] value);
    
    /**
     * Delegate the required 10419 data to Continua Agent.
     * 
     * @param type : The command of this segment data.
     *        Range: Valid object of ContinuaCommand.
     *        Unit: ContinuaCommand.
     *        Scaling: 1.
     * @param value : The target segment data.
     *        Range: Valid object of each segment data definition.
     *        Unit: SafetyByteArray.
     *        Scaling: 1.
     * 
     * return void [out]: None.
     */
    private native final void _SetContinuaDataOf10419(int type, byte[] value);
    
    /**
     * Delegate the required RPC data to Continua Agent.
     * 
     * @param value : The target segment data.
     *        Range: Valid object of each segment data definition.
     *        Unit: SafetyByteArray.
     *        Scaling: 1.
     * 
     * return void [out]: None.
     */
    private native final void _SetContinuaDataOfRPC(byte[] value);
    
    /**
     * Delegate the received data and signed signature to JNI to verify the data.
     * If pass return true, otherwise return false.
     *
     * @param keyType : The key which is used to verify the signature.
     *        Range: Refer to the definition of KeySelectorType.
     *        Unit: Integer.
     *        Scaling: 1.
     * @param original : The original data of transmission.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.
     * @param data : The signed signature data.
     *        Range: Valid object of byte[].
     *        Unit: byte[].
     *        Scaling: 1.     
     * 
     * return boolean [out] The result of verification of signed signature.
     *        Range: True or false.
     *        Unit: boolean.
     *        Scaling: 1.
     */
    public native final boolean _VerifySignedSignature(int keyType, byte[] original, byte[] data);
}
