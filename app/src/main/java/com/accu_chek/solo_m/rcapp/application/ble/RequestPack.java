package com.accu_chek.solo_m.rcapp.application.ble;

import java.util.ArrayList;

import android.os.Parcel;
import android.os.Parcelable;

import com.accu_chek.solo_m.rcapp.application.ble.constant.CommsConstant;
import com.accu_chek.solo_m.rcapp.application.exception.DataIntegrityException;
import com.accu_chek.solo_m.rcapp.application.util.ByteConverter;
import com.accu_chek.solo_m.rcapp.application.util.Debug;

/**
 * The RequestPack is a parcelable container for requests.
 * 
 * 
 */
public class RequestPack implements Parcelable
{
    private static final String TAG = "RequestPack";
    private int mGroupId = 0;
    private IRequest mRequest = null;

    public static final Parcelable.Creator<RequestPack> CREATOR = new Parcelable.Creator<RequestPack>()
    {

        public RequestPack createFromParcel(Parcel in)
        {
            RequestPack pack = null;
            try
            {
                pack= new RequestPack(in);
            }
            catch (ClassNotFoundException e)
            {

                e.printStackTrace();
            }
            finally
            {
                // Apply to the coding standard
            }
            return pack;
        }

        public RequestPack[] newArray(int size)
        {
            return new RequestPack[size];
        }
    };

    public RequestPack()
    {
    }

    public RequestPack(Parcel in) throws ClassNotFoundException
    {
        Class c = Class
                .forName("com.accu_chek.solo_m.rcapp.application.ble.IRequest");
        ClassLoader loader = c.getClassLoader();

        this.mGroupId = in.readInt();
        this.mRequest = in.readParcelable(loader);

    }

    public void setGroupId(int group)
    {

        this.mGroupId = group;

    }

    public int getGroupId()
    {
        return this.mGroupId;
    }

    public void setRequest(IRequest request)
    {

        this.mRequest = request;
    }

    public IRequest getRequest()
    {

        return this.mRequest;
    }

    /**
     * "It is used for for some validation code -- in particular to implement Bundle.hasFileDescriptors()"
     * (Dianne Hackborn -- Android framework engineer)
     * Not implemented in SoloM_RCAPP.
     */
    @Override
    public int describeContents()
    {

        return 0;
    }

    /**
     * Flatten this object in to a Parcel.
     * 
     * @param dest The Parcel in which the object should be written.
     * @param flags Additional flags about how the object should be written. May
     *            be 0 or PARCELABLE_WRITE_RETURN_VALUE.
     */
    @Override
    public void writeToParcel(Parcel dest, int flags)
    {
        dest.writeInt(this.mGroupId);
        dest.writeParcelable((Parcelable) this.mRequest, 0);
    }

    /**
     * The function supports CommandDispatcherServer.wrapFrame to generate frame
     * byte[].
     * 
     * @author EDLiu
     * @param
     * @param
     * 
     * 
     */

    public void writeToByteArrayList(ArrayList<byte[]> data, int flags)
    {

        int groupId = 0;
        int command = 0;
        command = getRequest().getCommand().get();
        groupId = allocateGroup(command);

        setGroupId(groupId);
        Debug.printD(TAG, "submitRequest = "
                + getRequest().getClass().getName());
        
        
        data.add(ByteConverter.getBytes((short) this.mGroupId));
        this.mRequest.writeToByteArrayList(data);
        // if (mRequest.getCommand() == CommsConstant.CommandCode.BT_ATTR_WRITE)

    }
    
    /**
     * 
     * Function Description
     *
     * @param command
     * @return
     * @return int [out] Delete pre line return if exist. Parameter Description
     */
    private int allocateGroup(int command)
    {

        if (CommsConstant.CommandGroup.COMMS.contains(command))
        {
            return CommsConstant.GroupId.COMM_PROCESSOR;
        }
        else if (CommsConstant.CommandGroup.BTLE.contains(command))
        {
            return CommsConstant.GroupId.BTLE;
        }
        else if (CommsConstant.CommandGroup.RECORD.contains(command))
        {
            return CommsConstant.GroupId.SERVICE_RECORD;
        }
        else
        {
            Debug.printD(
                    TAG,
                    "Group Id not allocated! command = "
                            + Integer.toHexString(command & 0xffff));
            throw new DataIntegrityException(
                    "Group Id not allocated! command = "
                            + Integer.toHexString(command & 0xffff));
        }

    }
    
   

}
// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
