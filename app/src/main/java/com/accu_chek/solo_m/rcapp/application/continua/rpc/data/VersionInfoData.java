package com.accu_chek.solo_m.rcapp.application.continua.rpc.data;

import java.util.LinkedList;
import java.util.List;

import org.apache.http.util.ByteArrayBuffer;

import com.accu_chek.solo_m.rcapp.application.continua.model.ParseUtils;
import com.accu_chek.solo_m.rcapp.application.safety.CRCTool;
import com.accu_chek.solo_m.rcapp.application.safety.SafetyByteArray;

public class VersionInfoData 
{
	public enum SpecType
	{
		UNSPECIFIED(0),
		SERIAL_NUMBER(1),
		PART_NUMBER(2),
		HW_REVISION(3),
		SW_REVISION(4),
		FW_REVISION(5),
		PROTOCOL_REVISION(6),
		PROD_SPEC_GMDN(7);
		
		/**
		 * The value of Spec type id.
		 */
		private final int ID;
		
		/**
		 * Put the id into this enumeration instance.
		 * 
		 * @param id : The value of Spec type id.
		 * 		  Range: Refer to the definition of SpectType.
		 * 		  Unit: Integer.
		 * 		  Scaling: 1.
		 */
		private SpecType(int id)
		{
			ID = id;
		}
		
		/**
		 * Return the id of SpectType.
		 * 
		 * see ID [out]
		 * 
		 * return int [out]: The value of Spec type id.
		 * 		  Range: Refer to the definition of SpecType.
		 * 		  Unit: Integer.
		 * 		  Scaling: 1.
		 */
		public int getId()
		{
			return ID;
		}
	}
	
	/**
	 * The list stores the version information of each component.
	 */
	private List<SafetyByteArray> mData = new LinkedList<SafetyByteArray>();
	
	/**
	 * Put the input version information into the version list.
	 * 
	 * @param type : The version type of input data.
	 * 		  Range: Refer to the definition of SpecType.
	 * 		  Unit: SpecType.
	 * 		  Scaling: 1. 
	 * @param componentId : The value of component id.
	 * 		  Range: -2^31 to (2^31)-1.
	 * 		  Unit: Integer.
	 * 		  Scaling: 1.
	 * @param value : The data of the version information.
	 * 		  Range: Valid object of byte[].
	 * 		  Unit: byte[].
	 * 		  Scaling: 1.
	 * 
	 * return VersionInfoData [out]: The instance contains the component info.
	 * 		  Range: Valid object of VersionInfoData.
	 * 		  Unit: VersionInfoData.
	 * 		  Scaling: 1.
	 */
	public VersionInfoData setVersionInfo(SpecType type, int componentId, byte[] value)
	{
		SafetyByteArray result = new SafetyByteArray();
		ByteArrayBuffer buffer = new ByteArrayBuffer(0);
		
		byte[] typeInBytes = ParseUtils.parseInt16(type.getId());
		byte[] idInBytes = ParseUtils.parseInt16(componentId);
		byte[] lengthInBytes = ParseUtils.parseInt16(value.length);
		
		buffer.append(typeInBytes, 0, typeInBytes.length);
		buffer.append(idInBytes, 0, idInBytes.length);
		buffer.append(lengthInBytes, 0, lengthInBytes.length);
		buffer.append(value, 0, value.length);
		
		result.set(buffer.toByteArray(), CRCTool.generateCRC16(buffer.toByteArray()));
		
		mData.add(result);
		
		return this;
	}
	
	/**
	 * Gather all component version data and convert to byte array.
	 * 
	 * return SafetyByteArray [out]: The data of all version information.
	 * 		  Range: Valid object of SafetyByteArray.
	 * 		  Unit: SafetyByteArray.
	 * 		  Scaling: 1.
	 */
	public SafetyByteArray generateBytes()
	{
		ByteArrayBuffer buffer = new ByteArrayBuffer(0);
		ByteArrayBuffer dataBuffer = new ByteArrayBuffer(0);
		
		int length = 0;
		byte[] lengthInBytes = null;
		byte[] countInBytes = ParseUtils.parseInt16(mData.size());
		
		buffer.append(countInBytes, 0, countInBytes.length);
		
		for (SafetyByteArray each : mData)
		{
			length += each.getByteArray().length;
			dataBuffer.append(each.getByteArray(), 0, each.getByteArray().length);
		}
		
		lengthInBytes = ParseUtils.parseInt16(length);
		buffer.append(lengthInBytes, 0, lengthInBytes.length);
		buffer.append(dataBuffer.toByteArray(), 0, dataBuffer.length());
		
		return ParseUtils.appendCRC(buffer.toByteArray());
	}
}
