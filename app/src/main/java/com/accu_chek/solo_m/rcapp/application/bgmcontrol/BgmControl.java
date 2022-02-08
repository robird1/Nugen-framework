/**
 * ===========================================================================
 * Copyright 2013 Roche Diagnostics GmbH
 * All Rights Reserved
 * ===========================================================================
 *
 * Class name: com.accu_chek.solo_m.rcapp.application.BGMControl
 * Brief:
 *
 * Create Date: 2/5/2015
 * $Revision: 25175 $
 * $Author: VictorChen $
 * $Id: BgmControl.java 25175 2015-11-30 11:25:57Z VictorChen $
 */

package com.accu_chek.solo_m.rcapp.application.bgmcontrol;

import java.util.ArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.RemoteException;

import com.accu_chek.solo_m.rcapp.application.CustJavaFrameworkManager;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.BgmBuildCommand.ParseResult;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception.BgmCommunicationTimeOutException;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception.BgmException;
import com.accu_chek.solo_m.rcapp.application.bgmcontrol.exception.RDYDessertException;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController;
import com.accu_chek.solo_m.rcapp.application.ble.blemanager.BLEController.ResponseCallback;
import com.accu_chek.solo_m.rcapp.application.ble.constant.ResponseAction;
import com.accu_chek.solo_m.rcapp.application.ble.response.BlankMessageResponse;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.CommonConstants;
import com.accu_chek.solo_m.rcapp.application.common.NugenFrameworkConstants.ProductionConstants;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ICustomizedHWManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol.LEDManager;
import com.accu_chek.solo_m.rcapp.application.customizedhwmanager.ledcontrol.LEDManager.LEDTYPE;
import com.accu_chek.solo_m.rcapp.application.emwrservice.EMWRList;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyMessage;
import com.accu_chek.solo_m.rcapp.application.emwrservice.NotifyProxy;
import com.accu_chek.solo_m.rcapp.application.exception.OperationFailException;
import com.accu_chek.solo_m.rcapp.application.usbconnection.IPCConnect;
import com.accu_chek.solo_m.rcapp.application.usbconnection.IUsbListener;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyBoolean;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyString;
import com.accu_chek.solo_m.rcapp.application.setting.NugenGeneralModel;
import com.accu_chek.solo_m.rcapp.application.settinginterface.NugenSettingModel;
import com.accu_chek.solo_m.rcapp.application.util.CommonUtils;
import com.accu_chek.solo_m.rcapp.application.util.Debug;
import com.accu_chek.solo_m.rcapp.application.util.IRCSystemPeoperty;
import com.accu_chek.solo_m.rcapp.communication.bgmcommunication.BgmCommunication;
import com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface.UARTManager;
import com.accu_chek.solo_m.rcapp.communication.customizedhwmanager.uartinterface.UARTManager.UARTPort;

public class BgmControl extends IBGMControl.Stub
{

    private static final String TAG = BgmControl.class.getSimpleName();
    /**
     * IBGMOnListener callback object, for UI screen action.
     * 
     * Range: valid object
     * Unit: IBGMOnListener
     * Scaling: 1
     */
    private static IBGMOnListener mListener = null;
    /**
     * IMeInformationListener callback object, for get bgm information.
     * 
     * Range: valid object
     * Unit: IMeInformationListener
     * Scaling: 1
     */
    private static IMeInformationListener mMeIndormationListener = null;

    /**
     * BgmCommunication object.
     * 
     * Range: valid object
     * Unit: BgmCommunication
     * Scaling: 1
     */
    private BgmCommunication mBgmCommunication = null;

    /**
     * BgmJni object.
     * 
     * Range: valid object
     * Unit: BgmJni
     * Scaling: 1
     */

    private BgmJni mBgmJni = null;

    /**
     * BgmControl object for singleton.
     * 
     * Range: valid object
     * Unit: BgmControl
     * Scaling: 1
     */
    private static BgmControl mInstance = null;

    /**
     * Constructor of BgmControl, save input context.
     * 
     * return void [out] None.
     * 
     * @param context Constructor of Bgm Control context
     *            Range: valid object of context
     *            Unit: Context
     *            Scaling: 1
     */

    protected BgmControl(Context context)
    {
        Debug.printI(TAG, "[BGMControl]: enter");
        CommonUtils.objectCheck(context);
        BgmUtils.setContext(context);
    }

    /**
     * Send ReadMeStatusCommand to check strip appearance.
     * 
     * In case when strip insert, call checkCurrentWorkFlow().
     * On the contrary, power down bgm.
     * 
     * return void [out] None.
     */

    public void checkBgmInterrupt()
    {
        Debug.printI(TAG, "CheckBgmStatus");
        boolean isPowerOnInterrupt = false;
        boolean isStripInsert = false;
        boolean isCheckStatus = false;
        try
        {
            // send bgm status command
            BgmBuildCommand getCommandPath = BuildCommandFactory
                    .create(IBgmConstant.CHECKBGMSTATUS);
            CommonUtils.objectCheck(getCommandPath);
            getCommandPath.buildCommand();
            // at initial power on bgm, after check status, set power on
            // interrupt
            // false.
            Debug.printI(TAG,
                    "Test result: " + CheckBgmStatus.getIsStripInsert());
            isPowerOnInterrupt = BgmUtils.getIsPowerOnInterrupt();
            isStripInsert = CheckBgmStatus.getIsStripInsert();
            isCheckStatus = (isPowerOnInterrupt == false) && isStripInsert;

            if (isCheckStatus)
            {
                checkMeterStatus();
            }
            else
            {
                powerDownBgm();
            }
            BgmUtils.setIsPowerOnInterrupt(false);
        }
        catch (BgmException ex)
        {
            Debug.printI(TAG,
                    "checkBGMInterrupt catch exception: " + ex.getMessage());
            errorHandling();
        }
        catch (RDYDessertException ex)
        {
            Debug.printI(TAG, "checkBgmInterrupt reset");
            mBgmJni.resetBgm();
        }
        catch (BgmCommunicationTimeOutException e)
        {
            Debug.printI(TAG, "checkBgmInterrupt reset");
            mBgmJni.resetBgm();
        }
        finally
        {
            // Empty for static code analysis
        }
    }

    /**
     * 
     * Get meter USB plug in flag, start up flag and EMWR status.
     * In case all the conditions are passed, check meter flow.
     * On the contrary, power down bgm and show EMWR.
     *
     * return void [out] None.
     */

    protected void checkMeterStatus()
    {
        boolean isInOtherWorkFlow = false;
        boolean otherWorkFlowConfirm = false;
        boolean isStartup = false;
        boolean isBatteryEnough = BgmUtils.getBatteryPower();
        final String PERSIST_SYS_START_UP = "persist.sys.system.isStartup";
        final String isStartrup = "true";
        boolean isInProductionMode = NugenSettingModel.getSafetyBoolean(
                BgmUtils.getContext(), ProductionConstants.KEY_INPRODUCTION) == SafetyBoolean.TRUE;
        IRCSystemPeoperty bpSetProperty = CustJavaFrameworkManager
                .getRCSystemPropertyService(null);

        boolean isPlugIn = BgmUtils.getIsUsbPlugIn();
        String startupFlag = "";
        CommonUtils.objectCheck(bpSetProperty);
        try
        {
            // get start up flag.
            startupFlag = bpSetProperty.getProperty(PERSIST_SYS_START_UP, "");
            isStartup = startupFlag.equals(isStartrup)
                    && (isInProductionMode == false);
            // in startup flow
            if (isStartup == true)
            {
                getOnlistener().onStartUp();
                powerDownBgm();
            }
            // USB plug in and not in production mode, show EMWR
            else if (isPlugIn == true)
            {
                NotifyMessage msg = new NotifyMessage(EMWRList.EMW46009);
                NotifyProxy.showEMWR(BgmUtils.getContext(), msg);
                powerDownBgm();
            }
            else if (isBatteryEnough == false)
            {
                NotifyMessage msg = new NotifyMessage(EMWRList.EMW48403);
                NotifyProxy.showEMWR(BgmUtils.getContext(), msg);
                powerDownBgm();
            }
            // TODO
            // Check EMWR
            else
            {
                isInOtherWorkFlow = BgmUtils.getCurrentWorkFlow(BgmUtils
                        .getContext());
                // if other work flow previous state is true and next state is
                // false, it means user confirm and start bg test
                // insert strip, play strip insert sound. And then trigger
                // again, show wait screen.
                otherWorkFlowConfirm = (false == isInOtherWorkFlow)
                        && (true == BgmUtils.getWorkFlowConfirm());

                if (true == otherWorkFlowConfirm)
                { // just for show waiting when user confirm other work
                  // flow.
                    getOnlistener().onStripInsert();
                }
                else
                { // when strip insert, play sound
                    getOnlistener().onDetectedStrip();
                }
                checkCurrentWorkFlow();
            }
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // Empty for static code analysis
        }
    }

    /**
     * Get current flag that is in bg test flow.
     * In other flow or start up flow, show information
     * In bg test flow, enable mdi mode.
     * 
     * return void [out] None.
     */
    protected void checkCurrentWorkFlow()
    {
        Debug.printI(TAG, "checkCurrentWorkFlow enter");

        boolean isInOtherWorkFlow = BgmUtils.getCurrentWorkFlow(BgmUtils
                .getContext());
        boolean isInMdiMode = false;
        SafetyString mode = NugenSettingModel.getString(BgmUtils.getContext(),
                CommonConstants.KEY_SYSTEM_MODE);
        RunBgTest runBgTest = new RunBgTest();
        // check the meter mode.
        if (mode == null)
        {
            isInMdiMode = false;
        }
        else
        {
            isInMdiMode = mode.getString().equals(IBgmConstant.MDI);
        }

        // record other work flow status.
        BgmUtils.setWorkFlowConfirm(isInOtherWorkFlow);
        try
        {
            // in other work flow, show information.
            if (isInOtherWorkFlow)
            {
                // In other workflow
                Debug.printI(TAG, "checkCurrentWorkFlow isInotherflow");
                getOnlistener().onCheckWorkFlow();
                powerDownBgm();
            }
            // in mdi mode, start bg test
            else if (isInMdiMode)
            {
                ExecutorService executor = Executors.newFixedThreadPool(1);
                executor.execute(runBgTest);
                executor.shutdown();
            }
            // in pump mode
            else
            {
                getOnlistener().onStripInsert();
                setMdiInit();
            }
        }
        catch (RemoteException e)
        {
            Debug.printI(TAG, "checkCurrentWorkFlow reset");
            mBgmJni.resetBgm();
        }
        finally
        {
            // Empty for static code analysis
        }
    }

    /**
     * Send power down bgm command.
     * 
     * return void [out] None.
     */

    public void powerDownBgm()
    {
        try
        {
            Debug.printI(TAG, "PowerDownBgm++");
            BgmBuildCommand getCommandPath = BuildCommandFactory
                    .create(IBgmConstant.POWERDOWNBGM);
            CommonUtils.objectCheck(getCommandPath);
            getCommandPath.buildCommand();

        }
        catch (BgmException e)
        {
            Debug.printI(TAG, "powerDownBgm catch exception: " + e.getMessage());
            errorHandling();
        }
        catch (RDYDessertException e)
        {
            Debug.printI(TAG, "powerDownBgm reset");
            mBgmJni.resetBgm();
        }
        catch (BgmCommunicationTimeOutException e)
        {
            Debug.printI(TAG, "checkBgmInterrupt reset");
            mBgmJni.resetBgm();
        }
        finally
        {
            // static code for analysis
        }

    }

    /**
     * 
     * In cast the meter is bounded and connect, sync pump data fist.
     * On the contrary, set meter enter mdi mode.
     *
     * return void [out] None.
     */
    private void setMdiInit()
    {

        Debug.printI(TAG, "setMdiInit");
        Context context = BgmUtils.getContext();
        SendCommsCommandTimeout task = new SendCommsCommandTimeout(context);
        boolean isBound = SafetyBoolean.TRUE == BLEController.getInstance(
                context).isBonded();
        // get is connect flag
        boolean isConnect = BLEController.getInstance().isConnected() == SafetyBoolean.TRUE;
        if (isBound && isConnect)
        {
            SystemSynCallBack systemSyncCallBack = new SystemSynCallBack(task);
            BLEController.getInstance(context)
                    .setSystemSync(systemSyncCallBack);
            // start timer
            // task.start(IBgmConstant.BLE_DELAY_TIME);
        }
        else
        {
            setMdiMode(true);
        }
    }

    /**
     * 
     * Call BLEControl to enable or disable MDI mode
     * 
     * return void [out] None.
     * 
     * @param state[in] enable mdi mode or disable.
     *            Range: true, enable mdi mode
     *            false, disable mdi mode
     *            Unit: boolean
     *            Scaling: 1
     */

    protected static void setMdiMode(boolean state)
    {
        Debug.printI(TAG, "setMdiMode: " + state);
        Context context = BgmUtils.getContext();
        SendCommsCommandTimeout task = new SendCommsCommandTimeout(context);
        BgmUtils.setIsMDIMode(state);
        SetMdiCallBack setMdiCallBack = new SetMdiCallBack(task);

        if (true == state)
        {
            BLEController.getInstance(context).setMDIMode(SafetyBoolean.TRUE,
                    setMdiCallBack);
            // start timer
            task.start(IBgmConstant.BLE_DELAY_TIME);
        }
        else
        {
            BLEController.getInstance(context).setMDIMode(SafetyBoolean.FALSE,
                    setMdiCallBack);
            task.start(IBgmConstant.BLE_DELAY_TIME);
        }
    }

    /**
     * Set bgm date and time and send bg test command.
     * After measurement, check Bg or Cg.
     * In case the result is cg, check control solution range.
     * 
     * return void [out] None.
     */

    public void startbGTest()
    {
        BgmUtils.setIsProcessingAction(false);
        boolean isCgResult = false;
        Debug.printI(TAG, "IsbGTestFlow: " + BgmUtils.getIsbGTestCommand());
        try
        {
            setDateData();
            setTimeData();
            BgmUtils.setIsbGTestFlow(true);
            BgmBuildCommand getCommandPath = BuildCommandFactory
                    .create(IBgmConstant.BGMEASUREMENT);
            CommonUtils.objectCheck(getCommandPath);
            getCommandPath.buildCommand();
            BgmUtils.setIsbGTestFlow(false);
            Debug.printI(TAG, "is Cg result" + BgMeasurement.getIsCgResult());
            isCgResult = BgMeasurement.getIsCgResult();
            if (isCgResult)
            {
                getCommandPath = BuildCommandFactory
                        .create(IBgmConstant.COMPOSECGCOMMAND);
                CommonUtils.objectCheck(getCommandPath);
                getCommandPath.buildCommand();
            }
            else
            {
                // Empty for static code analysis
            }
            powerDownBgm();
        }
        catch (BgmException ex)
        {
            Debug.printI(TAG, "startbGTest catch exception: " + ex.getMessage());
            boolean isInterruptCommand = BgmUtils.getCommandKey().equals(
                    ICommandType.InterruptCommand.getCommandKey());
            if (isInterruptCommand)
            {
                // in bg test and send interrupt command, shall wait 4 sec for
                // NAK.
                CommonUtils.sleep(IBgmConstant.BGM_CANCELCOMMAND_TIMEOUT);
            }
            errorHandling();
        }
        catch (RDYDessertException e)
        {
            Debug.printI(TAG, "startbGTest reset");
            mBgmJni.resetBgm();
        }
        catch (BgmCommunicationTimeOutException e)
        {
            NotifyMessage msg = new NotifyMessage(EMWRList.EMW46004);
            LEDManager led = (LEDManager) ICustomizedHWManager
                    .getSystemService(ICustomizedHWManager.LED_SERVICE);
            CommonUtils.objectCheck(led);
            try
            {
                led.closeLED(LEDTYPE.STRIP);
                NotifyProxy.showEMWR(BgmUtils.getContext(), msg);
                mBgmJni.resetBgm();
            }
            catch (OperationFailException fail)
            {
                fail.printStackTrace();
            }
            finally
            {
                // static code for analysis
            }
            mBgmJni.resetBgm();
        }
        finally
        {
            // check system mode.
            boolean isInMdiMode = false;
            SafetyString mode = NugenSettingModel.getString(
                    BgmUtils.getContext(), CommonConstants.KEY_SYSTEM_MODE);
            if (mode == null)
            {
                isInMdiMode = false;
            }
            else
            {
                isInMdiMode = mode.getString().equals(IBgmConstant.MDI);
            }

            if (isInMdiMode == false)
            {
                setMdiMode(false);
            }
        }

        Debug.printI(TAG, "startbGTest exit");
    }

    /**
     * 
     * Send ReadErrorStatusCommand and power down bgm.
     * 
     * return void [out] None.
     */
    protected void errorHandling()
    {
        Debug.printI(TAG, "errorHnadling enter");
        // FIXME
        // wait sendcommand exit
        // avoid calling communication function at the same time.
        CommonUtils.sleep(IBgmConstant.waitErrorHandlingTime);
        try
        {
            BgmBuildCommand getCommandPath = BuildCommandFactory
                    .create(IBgmConstant.HANDLEERROR);
            CommonUtils.objectCheck(getCommandPath);
            getCommandPath.buildCommand();

        }
        catch (BgmException ex)
        {
            Debug.printI(TAG, "errorHandling BgmException reset");
            mBgmJni.resetBgm();
        }
        catch (RDYDessertException e)
        {
            Debug.printI(TAG, "errorHandling RDYDessertException reset");
            mBgmJni.resetBgm();
        }
        catch (BgmCommunicationTimeOutException e)
        {
            Debug.printI(TAG, "errorHandling BgmException reset");
            mBgmJni.resetBgm();
        }
        finally
        {
            // Empty for static code analysis
        }
    }

    /**
     * 
     * According command to call command class. Use BgmUtils.getCommandKey() and
     * use ParseBgmFactory to get command class path.
     * Then call the class to parse context.
     *
     * return void [out] None.
     * 
     * @param onResult
     *            Range: valid object
     *            Unit: ParseResult
     *            Scaling: 1
     */
    protected void parseBgmData(ParseResult onResult)
    {
        Debug.printI(TAG, "parseBGMData enter");
        String commnad = null;
        int size = mBgmCommunication.getReceiveData().size();
        commnad = BgmUtils.getCommandKey();
        Debug.printI(TAG, "[parseBGMData]commnadkey: " + commnad + "size "
                + size);
        // print result of mReceiveBgmData
        // FIXME, just for debug
        // ====
        if (size > 0)
        {
            for (Byte e : mBgmCommunication.getReceiveData())
            {
                Debug.printI(TAG, toHex(e));
            }
        }
        // ====
        ParseBgmProcess parseBgmCommand = ParseBgmFactory
                .createBgmparse(commnad);
        if (parseBgmCommand != null)
        {
            parseBgmCommand.parse(onResult);
        }
        else
        {
            // static code for analysis
        }
        Debug.printI(TAG, "parseBGMData exit");
    }

    /**
     * 
     * Send commandList to BgmCommunication module.
     *
     * return void [out] None.
     * 
     * @param commandList [in] command list
     *            Range: valid object
     *            Unit: ArrayList<ICommandType>
     *            Scaling: 1
     * @param onResult [in] callback object
     *            Range: valid object
     *            Unit: ParseResult
     *            Scaling: 1
     * @throws RDYDessertException
     * @throws BgmException, receive NAK, do error handling.
     * @throws BgmCommunicationTimeOutException
     * 
     */

    public synchronized void sendCommand(ArrayList<ICommandType> commandList,
            ParseResult onResult) throws RDYDessertException, BgmException,
            BgmCommunicationTimeOutException
    {
        String commandKey = null;
        String bgCommandKey = ICommandType.BgTestCommand.getCommandKey();
        byte[] command = null;
        byte[] bgmData = null;
        boolean isBgTestCommand = false;

        CommonUtils.objectCheck(commandList);
        Debug.printI(TAG,
                "sendCommand: Commandlist size = " + commandList.size());

        // check RDY status.
        int RDY = mBgmJni.checkRdyStatus();

        if (IBgmConstant.READY_PIN_STATUS_VALUE != RDY)
        {
            throw new RDYDessertException();
        }
        else
        {
            for (ICommandType commandType : commandList)
            {
                commandKey = commandType.getCommandKey();
                BgmUtils.setCommandKey(commandKey);
                isBgTestCommand = commandKey.equals(bgCommandKey);
                Debug.printI(TAG, "sendCommand: commandID = " + commandKey);
                Debug.dumpPayload(commandKey, (byte) 0x00);
                if (isBgTestCommand)
                {
                    // set flag about bg test
                    BgmUtils.setIsbGTestCommand(true);
                }
                else
                {
                    BgmUtils.setIsbGTestCommand(false);
                }

                command = commandType.getCommand();
                bgmData = commandType.getByteData();
                // call bgm communication

                BgmCommunication.getInstance()
                        .writeBgmCommand(command, bgmData);

                // command parsing
                parseBgmData(onResult);

            }
            Debug.printI(TAG, "sendCommand Exit ");
        }
    }

    /**
     * Singleton of BgmControl class
     * 
     * return BGM control object
     * Range: valid object
     * Unit: BgmControl
     * Scaling: 1
     * 
     * @param context[in] Constructor of Bgm control context
     *            Range: valid object
     *            Unit: Context
     *            Scaling: 1
     * 
     */
    public static synchronized BgmControl getInstance(Context context)
    {
        if (null == mInstance)
        {
            CommonUtils.objectCheck(context);
            mInstance = new BgmControl(context);
        }
        else
        {
            // Empty for static code analysis
        }
        return mInstance;
    }

    /**
     * 
     * Abort bg test process, when Usb plug in.
     * Send interrupt command and show USB plug in information.
     * 
     * return void [out] None.
     */

    public void abortbGTest()
    {
        Debug.printI(TAG, "abortbGTest ++");
        SafetyByteArray data = null;

        byte[] sendcommand = new byte[1];
        sendcommand = ICommandType.InterruptCommand.getCommand();
        BgmUtils.setCommandKey(ICommandType.InterruptCommand.getCommandKey());
        data = new SafetyByteArray(sendcommand,
                CRCTool.generateCRC16(sendcommand));
        UARTManager Uartdata;
        try
        {
            getOnlistener().onUsbConnect();
            Uartdata = (UARTManager) ICustomizedHWManager
                    .getSystemService(ICustomizedHWManager.UART_SERVICE);
            CommonUtils.objectCheck(Uartdata);
            Uartdata.send(UARTPort.BGMUART, data);
            Debug.dumpPayload("W", (byte) sendcommand[0]);
        }
        catch (OperationFailException e)
        {
            // call EMWR
            e.printStackTrace();
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // Empty for static code analysis
        }

    }

    /**
     * Compose time data of SetTimeCommand.
     *
     * see ICommandType.Set_Time_Command[in]
     * 
     * return void [out] None.
     */

    protected void setTimeData()
    {
        byte[] timeCommand = BgmUtils
                .setDateTimeFormat(IBgmConstant.TIMEFORMAT);
        ICommandType.SetTimeCommand.setByteData(timeCommand);
    }

    /**
     * 
     * Compose date data of SetDateCommand.
     * 
     * see ICommandType.Set_Date_Command[in]
     * 
     * return void [out] None.
     */
    protected void setDateData()
    {
        byte[] dateCommand = BgmUtils
                .setDateTimeFormat(IBgmConstant.DATEFORMAT);
        ICommandType.SetDateCommand.setByteData(dateCommand);
    }

    // just for log out byte data
    private static String toHex(byte b)
    {
        int shift = 4;
        byte data = 0xf;
        String tohex = "" + "0123456789ABCDEF".charAt(data & b >> shift)
                + "0123456789ABCDEF".charAt(b & data);
        return tohex;
    }

    /**
     * Initial bgm control module power on Bgm submodule, register USB listener
     * and set POST flag false.
     * 
     * see bgmCommunication[out]
     * see bgmJni[out]
     * return void [out] None.
     */
    @Override
    public void initBgm() throws RemoteException
    {

        // initial bg cmd array list
        Debug.printI(TAG, "[initBgm] enter");

        BgmErrorCodeTable.initialErrorCodeTable();

        NugenGeneralModel.setSafetyBoolean(BgmUtils.getContext(),
                NugenFrameworkConstants.BGMConstants.KEY_BG_POST,
                SafetyBoolean.FALSE);
        mBgmCommunication = BgmCommunication.getInstance();
        // bgmCommunication.startReadBgmData();
        mBgmJni = BgmJni.getInstance();
        registerUsbListener();
        BgmUtils.setIsPowerOnInterrupt(true);
        CommonUtils.sleep(3500);
        mBgmJni.powerOnBgm();

    }

    /**
     * Set IBGMOnListener callback object.
     * see mListener[out]
     * return void [out] None.
     * 
     * @param listener [in] IBGMOnListener class listener
     *            Range: valid object of IBGMOnListener
     *            Unit: IBGMOnListener
     *            Scaling:1
     * 
     */
    @Override
    public void setOnListener(IBGMOnListener listener)
    {
        CommonUtils.objectCheck(listener);
        mListener = listener;
    }

    /**
     * 
     * Set setIMeInformationListener object
     * see mMeIndormationListener[in]
     * return void [out] None.
     * 
     * @param MeInformationListener
     *            Range: valid object of IMeInformationListener
     *            Unit: IMeInformationListener
     *            Scaling:1
     */
    private static void setIMeInformationListener(
            IMeInformationListener MeInformationListener)
    {
        mMeIndormationListener = MeInformationListener;
    }

    /**
     * 
     * Get IBGMOnListener call back object.
     * see mListener[in]
     *
     * return IBGMOnListener [out]
     * Range: valid object
     * Unit: IMeInformationListener
     * Scaling:1
     * 
     */
    public IBGMOnListener getOnlistener()
    {
        IBGMOnListener listener;
        CommonUtils.objectCheck(mListener);
        listener = mListener;
        return listener;
    }

    /**
     * 
     * Get IMeInformationListener call back object.
     * 
     * return IMeInformationListener [out]
     * Range: valid object of IMeInformationListener
     * Unit: IMeInformationListener
     * Scaling:1
     */
    protected static IMeInformationListener getIMeInformationListener()
    {
        return mMeIndormationListener;
    }

    /**
     * 
     * Get bgm submodule information. For time manager and RPC.
     * 
     * return void [out] None.
     * 
     * @param action [in]
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     * @param MeInformationlistener [in]
     *            Range: valid object
     *            Unit: MeInformationlistener
     *            Scaling: 1
     */

    @Override
    public void getMeInformation(String action,
            IMeInformationListener MeInformationlistener)
    {
        doMeInformationAciton(action, MeInformationlistener);
    }

    /**
     * 
     * Do getMeInformation action.
     *
     * return void [out] None.
     * 
     * @param action [in]
     *            Range: valid object
     *            Unit: String
     *            Scaling: 1
     * @param MeInformationlistener [in]
     *            Range: valid object
     *            Unit: MeInformationlistener
     *            Scaling: 1
     */
    private synchronized void doMeInformationAciton(String action,
            IMeInformationListener MeInformationlistener)
    {
        Debug.printI(TAG, "getMeInformation enter " + action + " "
                + MeInformationlistener);
        Debug.printI(
                TAG,
                "BgmUtils.getIsWakeUpInterrupt()0"
                        + BgmUtils.getIsWakeUpInterrupt());
        BgmUtils.setIsWakeUpInterrupt(true);
        Debug.printI(
                TAG,
                "BgmUtils.getIsWakeUpInterrupt()1"
                        + BgmUtils.getIsWakeUpInterrupt());
        CommonUtils.objectCheck(action, MeInformationlistener);
        setIMeInformationListener(MeInformationlistener);
        wakeUpBgm();
        // wait RDY assert
        CommonUtils.sleep(IBgmConstant.BGM_WAIT_RDYPIN);
        Debug.printI(TAG, "RDY_PIN" + mBgmJni.checkRdyStatus());
        try
        {
            BgmBuildCommand getCommandPath = BuildCommandFactory.create(action);
            if (getCommandPath != null)
            {
                getCommandPath.buildCommand();
            }
            else
            {
                MeInformationlistener.onError();
            }
        }
        catch (BgmException ex)
        {
            Debug.printI(TAG,
                    "getMeInformation catch exception: " + ex.getMessage());
            errorHandling();
        }
        catch (RDYDessertException e)
        {
            Debug.printI(TAG, "getMeInformation reset");
            mBgmJni.resetBgm();
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
        }
        catch (BgmCommunicationTimeOutException e)
        {
            Debug.printI(TAG, "getMeInformation reset");
            mBgmJni.resetBgm();
        }
        finally
        {
            Debug.printI(
                    TAG,
                    "BgmUtils.getIsWakeUpInterrupt()2"
                            + BgmUtils.getIsWakeUpInterrupt());
            BgmUtils.setIsWakeUpInterrupt(false);
            Debug.printI(
                    TAG,
                    "BgmUtils.getIsWakeUpInterrupt()3"
                            + BgmUtils.getIsWakeUpInterrupt());
            setIMeInformationListener(null);
        }
    }

    /**
     * wake up bgm, call BgmJni wakeUpBgm()
     * 
     * return void [out] None.
     */
    @Override
    public void wakeUpBgm()
    {
        Debug.printI(TAG, "[wakeUpBgm] enter");
        mBgmJni.wakeUpBgm();
    }

    /**
     * Update Bgm submodule code key data.
     * return void [out] None.
     * 
     * @param codeKeyData [in] code key data.
     *            Range: valid object of codeKeyData
     *            Unit: SafetyByteArray
     *            Scaling: 1
     * @throws RemoteException
     */

    @Override
    public void updateCodeKey(SafetyByteArray codeKeyData,IMeInformationListener MeInformationlistener)
            throws RemoteException
    {
        Debug.printI(TAG, "updateCodeKey ++");
        CommonUtils.objectCheck(codeKeyData,MeInformationlistener );
        setIMeInformationListener(MeInformationlistener);
        BgmUtils.setIsWakeUpInterrupt(true);
        BgmJni.getInstance().wakeUpBgm();
        CommonUtils.sleep(IBgmConstant.BGM_WAIT_RDYPIN);

        // wait RDY assert
        ArrayList<ICommandType> commandList = new ArrayList<ICommandType>();
        ICommandType.UpdateCodeKeyCommand.setByteData(codeKeyData
                .getByteArray());

        try
        {
            commandList.clear();
            commandList.add(ICommandType.ClearStatusCommand);
            commandList.add(ICommandType.ReadEventINTCommand);
            commandList.add(ICommandType.AccessCommand);
            commandList.add(ICommandType.UpdateCodeKeyCommand);
            commandList.add(ICommandType.PowerDownCommand);
            sendCommand(commandList, null);
        }
        catch (RDYDessertException e)
        {
            e.printStackTrace();
        }
        catch (BgmException e)
        {
            errorHandling();
        }
        catch (BgmCommunicationTimeOutException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // static code for analysis.
        }
        BgmUtils.setIsWakeUpInterrupt(false);
        Debug.printI(TAG, "updateCodeKey --");
    }

    /**
     * 
     * Register usb listener.
     * 
     * return void [out] None.
     *
     */
    protected void registerUsbListener()
    {
        UsbListener usbListener = new UsbListener();
        IPCConnect pcConnect = CustJavaFrameworkManager.getPCConnectService();

        // Null Object Check
        CommonUtils.objectCheck(pcConnect);
        try
        {
            pcConnect.setOnUsbListener((IUsbListener) usbListener);
        }
        catch (RemoteException e)
        {
            e.printStackTrace();
        }
        finally
        {
            // Apply to the coding standard
        }

    }

    private static final class RunBgTest implements Runnable
    {
        /**
         * 
         * Call startbGTest() to do bg measurement
         * 
         * return void [out] None.
         */
        @Override
        public void run()
        {
            BgmControl.mInstance.startbGTest();
        }

    }

    private static final class SystemSynCallBack implements ResponseCallback
    {
        /**
         * SendCommsCommandTask object.
         */
        private SendCommsCommandTimeout mTask = null;

        /**
         * SetMdiCallBack class constructor.
         * 
         * return void [out] None.
         * 
         * @param task[in] SendCommsCommandTask object
         *            Range: valid object of SendCommsCommandTask
         *            Unit: SendCommsCommandTask
         *            Scaling: 1
         * 
         */
        public SystemSynCallBack(SendCommsCommandTimeout task)
        {
            mTask = task;
        }

        /**
         * 
         * Set sync commas command call back function.
         * 
         *
         * @param result
         */
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {

            // stop timer
            mTask.cancel();

            IntentFilter filter = new IntentFilter();
            filter.addAction(BlankMessageResponse.class.getName());
            filter.addAction(ResponseAction.CommandResponse.BT_SYSTEM_SYNC);
            Context context = BgmUtils.getContext().getApplicationContext();
            context.registerReceiver(new BroadcastReceiver()
            {

                @Override
                public void onReceive(Context context, Intent intent)
                {
                    String action = intent.getAction();
                    if (ResponseAction.CommandResponse.BT_SYSTEM_SYNC
                            .equalsIgnoreCase(action))
                    {
                        boolean isUsbPlugIn = false;

                        isUsbPlugIn = BgmUtils.getIsUsbPlugIn();
                        if (false == isUsbPlugIn)
                        {
                            setMdiMode(true);
                        }
                        context.unregisterReceiver(this);
                    }

                }

            }, filter);

        }
    }

    private static final class SystemSynCall extends BroadcastReceiver
    {

        /**
         * 
         *
         * @param context
         * @param intent
         */

        @Override
        public void onReceive(Context context, Intent intent)
        {
            String action = intent.getAction();
            if (ResponseAction.CommandResponse.BT_SYSTEM_SYNC
                    .equalsIgnoreCase(action))
            {
                boolean isUsbPlugIn = false;

                isUsbPlugIn = BgmUtils.getIsUsbPlugIn();
                if (false == isUsbPlugIn)
                {
                    setMdiMode(true);
                }
                context.unregisterReceiver(this);
            }

        }

    }

    private static final class SetMdiCallBack implements ResponseCallback
    {
        /**
         * SendCommsCommandTask object.
         */
        private SendCommsCommandTimeout mTask = null;

        /**
         * SetMdiCallBack class constructor.
         * 
         * return void [out] None.
         * 
         * @param task[in] SendCommsCommandTask object
         *            Range: valid object of SendCommsCommandTask
         *            Unit: SendCommsCommandTask
         *            Scaling: 1
         * 
         */
        public SetMdiCallBack(SendCommsCommandTimeout task)
        {
            CommonUtils.objectCheck(task);
            mTask = task;
        }

        /**
         * 
         * Set mdi command call back function.
         * 
         *
         * @param result
         */
        @Override
        public void onRequestCompleted(SafetyBoolean result)
        {
            boolean isResponseTure = SafetyBoolean.TRUE.getByte() == result
                    .getByte();

            boolean isEnableMdi = false;

            boolean isUsbPlugIn = false;

            boolean isStartMeasurement = false;

            if (isResponseTure)
            {
                // stop timmer
                mTask.cancel();

                isEnableMdi = BgmUtils.getIsMDIMode();
                isUsbPlugIn = BgmUtils.getIsUsbPlugIn();
                Debug.printD(TAG, "inEnableMdi " + isEnableMdi);

                isStartMeasurement = (isEnableMdi && (isUsbPlugIn == false));

                RunBgTest runBgTest = new RunBgTest();

                // set mdi true and usb not plug in, start bg test
                if (isStartMeasurement)
                {
                    Debug.printD(TAG, "setMdiMode true call back start bg");
                    // stop timer

                    ExecutorService executor = Executors.newFixedThreadPool(1);
                    executor.execute(runBgTest);
                    executor.shutdown();
                }
                // disable mdi
                else if (isEnableMdi == false)
                {
                    Debug.printD(TAG, "setMdiMode false call back");
                }
                // set mdi true but usb not plug in, disable mdi mode.
                else
                {
                    try
                    {
                        BgmControl.getInstance(null).getOnlistener()
                                .onUsbConnect();
                        Debug.printD(TAG, "setMdiMode call back Usb plug in");
                    }
                    catch (RemoteException e)
                    {
                        e.printStackTrace();
                    }
                    finally
                    {
                        setMdiMode(false);
                    }
                }
            }
            else
            {
                // set mdi fail, show EMWR
                NotifyMessage msg = new NotifyMessage(EMWRList.EMW46013);
                LEDManager LED = (LEDManager) ICustomizedHWManager
                        .getSystemService(ICustomizedHWManager.LED_SERVICE);
                CommonUtils.objectCheck(LED);
                try
                {
                    LED.closeLED(LEDTYPE.STRIP);

                    NotifyProxy.showEMWR(BgmUtils.getContext(), msg);
                    BgmControl.getInstance(null).powerDownBgm();
                }
                catch (OperationFailException e)
                {
                    e.printStackTrace();
                }
                finally
                {
                    // static code for analysis
                }
            }
            Debug.printD(TAG, "onRequestCompleted exit");
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
// (R20520 2015-10-01 07:04:11 DWYang)
// ----------------------------------------------------------------------------
// add header footer.
// (R22322 2015-10-22 04:10:30 WilliyChiang)
// ----------------------------------------------------------------------------
// add updateCodeKey function.
// (R22746 2015-10-28 06:11:38 VictorChen)
// ----------------------------------------------------------------------------
// Refine code and comment.
// (R23130 2015-11-03 08:03:45 VictorChen)
// ----------------------------------------------------------------------------
// Refine code and comment.
// (R23259 2015-11-04 06:20:40 VictorChen)
// ----------------------------------------------------------------------------
// Fix NSIQ 60
// (R23259 2015-11-04 06:20:40 VictorChen)
// ----------------------------------------------------------------------------
// Fix NSIQ 60 issue.
// (R23452 2015-11-05 07:45:04 VictorChen)
// ----------------------------------------------------------------------------
// add broadcast receiver for ble SystemSyn command.
// (R23867 2015-11-11 05:16:12 VictorChen)
// ----------------------------------------------------------------------------
// Refine code comment.
// (R23877 2015-11-11 05:56:39 VictorChen)
// ----------------------------------------------------------------------------
// Add flag to check in Bg test flow.
// (R24379 2015-11-18 23:07:09 henrytso)
// ----------------------------------------------------------------------------
// Refine code comment.
// (R24486 2015-11-20 01:48:56 VictorChen)
// ----------------------------------------------------------------------------
// Refine code comment.
// (R24899 2015-11-26 01:40:03 VictorChen)
// ----------------------------------------------------------------------------
// Add synchronized in MeInformation()
// (R24931 2015-11-26 05:23:59 VictorChen)
// ----------------------------------------------------------------------------
// Add update code key command call back.
