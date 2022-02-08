package com.accu_chek.solo_m.rcapp.application.ble;



import android.os.Parcel;
import android.os.Parcelable;

/**
 * The response pack carries a response of the request.
 */
public class ResponsePack implements Parcelable
{

    private int mGroupId = 0;
    private IResponse mResponse = null;

    public static final Parcelable.Creator<ResponsePack> CREATOR = new Parcelable.Creator<ResponsePack>()
    {
        public ResponsePack createFromParcel(Parcel in)
        {
            ResponsePack pack = null;
            try
            {
                pack = new ResponsePack(in);
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

        public ResponsePack[] newArray(int size)
        {
            return new ResponsePack[size];
        }
    };

    public ResponsePack()
    {
    };

    public ResponsePack(Parcel in) throws ClassNotFoundException
    {

        Class c = Class
                .forName("com.accu_chek.solo_m.rcapp.application.ble.IResponse");
        ClassLoader loader = c.getClassLoader();

        this.mGroupId = in.readInt();
        this.mResponse = in.readParcelable(loader);
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
        dest.writeParcelable((Parcelable) this.mResponse, 0);

    }

    public void setResponse(IResponse response)
    {
        this.mResponse = response;

    }

    public void setGroupId(int group)
    {
        this.mGroupId = group;

    }

    public IResponse getResponse()
    {

        return this.mResponse;
    }

    public int getGroupId()
    {
        return this.mGroupId;
    }

}
// [BT] Add header and footer for BLEUC02.java
// [BT] Fixed Klocwork issue.
