
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <ap_int.h>

////////////////////////////////////////////20181229 n4m32  v2 without input and reorg opt ok input opt ok combine input relu comb ok // input opt ok // output opt ok //weight opt ok (5)n4m32i4o2 ok start
#define MAX(x,y) ((x)>(y)?(x):(y))
#define MIN(x,y) ((x)<(y)?(x):(y))
#define S 2
#define K 3

#define Tn 4
#define Tm 32
#define Tr 26
#define Tc 26

#define OnChipIB_Width  ((Tc-1)*S+K)
#define OnChipIB_Height ((Tr-1)*S+K)
#define MAX_BETA_LENGTH (1024)
#define INTERWIDTH 20

typedef unsigned char UCHAR;

void mmcpy_inputport(int *input,int input_memcpy_buffer[(OnChipIB_Width+3)/2],ap_uint<3> TN_MIN,int RowOffset,UCHAR RowIntNum)
{
	bool enable = TN_MIN > 0;
	if(!enable)
		return;

	memcpy(input_memcpy_buffer,(int *)(input + RowOffset),RowIntNum*sizeof(int));

}

void mmcpy_inputport1(int *input,int input_memcpy_buffer[(OnChipIB_Width+3)/2],ap_uint<3> TN_MIN,int RowOffset,UCHAR RowIntNum)
{
	bool enable = TN_MIN > 1;
	if(!enable)
		return;

	memcpy(input_memcpy_buffer,(int *)(input + RowOffset),RowIntNum*sizeof(int));

}

void mmcpy_inputport2(int *input,int input_memcpy_buffer[(OnChipIB_Width+3)/2],ap_uint<3> TN_MIN,int RowOffset,UCHAR RowIntNum)
{
	bool enable = TN_MIN > 2;
	if(!enable)
		return;

	memcpy(input_memcpy_buffer,(int *)(input + RowOffset),RowIntNum*sizeof(int));


}

void mmcpy_inputport3(int *input,int input_memcpy_buffer[(OnChipIB_Width+3)/2],ap_uint<3> TN_MIN,int RowOffset,UCHAR RowIntNum)
{
	bool enable = TN_MIN > 3;
	if(!enable)
		return;

	memcpy(input_memcpy_buffer,(int *)(input + RowOffset),RowIntNum*sizeof(int));

}

void mmcpy_inputpixel_m2b_comb(int *input,int *input1,int *input2,int *input3,
						  int input_memcpy_buffer[(OnChipIB_Width+3)/2],int input_memcpy_buffer1[(OnChipIB_Width+3)/2],
						  int input_memcpy_buffer2[(OnChipIB_Width+3)/2],int input_memcpy_buffer3[(OnChipIB_Width+3)/2],
						  ap_uint<1>  RowBeginByte[Tn],ap_uint<3> TN_MIN_3b,ap_uint<6> t2,ap_uint<1> RowSub,int IN_OFFSET,ap_uint<9> RowIncreaseLength,ap_uint<18> IHxIW_18b,ap_uint<6> ColIncreaseLength,ap_uint<6> next_t2[1],bool next_IsRowPixel[1],bool IsRowPixel,bool enable)
{
	static int tmp_inoffset;

	next_t2[0] = t2;
	next_IsRowPixel[0] = IsRowPixel;

	if(!enable)
		return;

	bool init = (t2==0);
	if(init)
	{
		tmp_inoffset = IN_OFFSET;
	}else
	{
		tmp_inoffset += RowIncreaseLength;
	}

	int InOffset[Tn];
#pragma HLS ARRAY_PARTITION variable=InOffset complete dim=1
	int RowOffset[Tn];
#pragma HLS ARRAY_PARTITION variable=RowOffset complete dim=1
	ap_uint<1>  LowBit[Tn];
#pragma HLS ARRAY_PARTITION variable=LowBit complete dim=1
	UCHAR BeginByteNum[Tn];
#pragma HLS ARRAY_PARTITION variable=BeginByteNum complete dim=1
	UCHAR RowIntNum[Tn];
#pragma HLS ARRAY_PARTITION variable=RowIntNum complete dim=1

	int t1;
	for(t1 = 0;t1 < Tn;t1++)
	{
#pragma HLS UNROLL
		InOffset[t1] = tmp_inoffset + t1*IHxIW_18b;
		RowOffset[t1] = InOffset[t1] >> 1;
		LowBit[t1] = InOffset[t1]&0x1;
		RowBeginByte[t1] = LowBit[t1];
		BeginByteNum[t1] = ColIncreaseLength + LowBit[t1];

//		assert((BeginByteNum[t1] > 0)&&(BeginByteNum[t1] < 256));

		RowIntNum[t1] = BeginByteNum[t1] >> 1;
		if(BeginByteNum[t1]&0x1)
			RowIntNum[t1]++;

//		assert((RowIntNum[t1] > 0)&&(RowIntNum[t1] < 256));
	}

	mmcpy_inputport(input,input_memcpy_buffer, TN_MIN_3b,RowOffset[0],RowIntNum[0]);
	mmcpy_inputport1(input1,input_memcpy_buffer1, TN_MIN_3b,RowOffset[1],RowIntNum[1]);
	mmcpy_inputport2(input2,input_memcpy_buffer2, TN_MIN_3b,RowOffset[2],RowIntNum[2]);
	mmcpy_inputport3(input3,input_memcpy_buffer3, TN_MIN_3b,RowOffset[3],RowIntNum[3]);
}

void copy_input2buf_row(short input_buffer[Tn][OnChipIB_Height][OnChipIB_Width],ap_uint<6> row_len,ap_uint<6> col_len,ap_uint<1> RowSub,ap_uint<1> ColSub,
					int input_memcpy_buffer[(OnChipIB_Width+3)/2],int input_memcpy_buffer1[(OnChipIB_Width+3)/2],
					int input_memcpy_buffer2[(OnChipIB_Width+3)/2],int input_memcpy_buffer3[(OnChipIB_Width+3)/2],
					ap_uint<1>  RowBeginByte[Tn],UCHAR TRow,UCHAR TCol,int LayerType,ap_uint<6> next_t2[1],bool next_enable[1],bool enable,ap_uint<3> T2Rate)
{

	if(!enable)
		return;

	static ap_uint<6> t2_local = 0;
	ap_uint<6> t2 = next_t2[0];
	bool IsRowPixel = next_enable[0];
	int t1,t3;
	ap_uint<6> t2r;
	ap_uint<3> T2R;

	bool initial = (t2==0);
	if(initial)
	{
		t2_local = 0;
	}

	short pad_value = 0;
	if(LayerType==1)
		pad_value = 0x8001;

	int input_mmcpy_offset[Tn];
#pragma HLS ARRAY_PARTITION variable=input_mmcpy_offset complete dim=1
	bool NextInputFlag[Tn];
#pragma HLS ARRAY_PARTITION variable=NextInputFlag complete dim=1
	ap_uint<1>  cnt[Tn];
#pragma HLS ARRAY_PARTITION variable=cnt complete dim=1
	short input_array[Tn][2];
#pragma HLS ARRAY_PARTITION variable=input_array complete dim=1

	for(t1 = 0;t1 < Tn; t1++)
	{
#pragma HLS UNROLL
		input_mmcpy_offset[t1] = 0;
	}

	if(!IsRowPixel)
	{
		T2R = T2Rate + 1;
	}else
	{
		T2R = T2Rate;
	}
	ap_uint<6> T2R_bound = MIN(t2_local + T2R,OnChipIB_Height);

	bool IsRowInit_flag = true;

	for(t2r = t2_local;t2r < T2R_bound; t2r++)
		for(t3 = 0;t3 < TCol; t3++)
		{
#pragma HLS PIPELINE
			bool IsRowPixel_t2r = (t2r >= RowSub)&&(t2r < (row_len + RowSub));
			bool IsColPixel = (t3 >= ColSub)&&(t3 < (col_len + ColSub));
			bool IsRowInit = (t3==ColSub)&&IsRowInit_flag;

			if(IsRowPixel_t2r&&IsColPixel)
			{
				if(IsRowInit)
				{
					IsRowInit_flag = false;
					cnt[0] = RowBeginByte[0];
					cnt[1] = RowBeginByte[1];
					cnt[2] = RowBeginByte[2];
					cnt[3] = RowBeginByte[3];
					NextInputFlag[0] = true;
					NextInputFlag[1] = true;
					NextInputFlag[2] = true;
					NextInputFlag[3] = true;
				}

				if(NextInputFlag[0])
				{
					input_array[0][0] = input_memcpy_buffer[input_mmcpy_offset[0]];
					input_array[0][1] = input_memcpy_buffer[input_mmcpy_offset[0]] >> 16;
					input_mmcpy_offset[0]++;
					NextInputFlag[0] = false;
				}

				if(NextInputFlag[1])
				{
					input_array[1][0] = input_memcpy_buffer1[input_mmcpy_offset[1]];
					input_array[1][1] = input_memcpy_buffer1[input_mmcpy_offset[1]] >> 16;
					input_mmcpy_offset[1]++;
					NextInputFlag[1] = false;
				}

				if(NextInputFlag[2])
				{
					input_array[2][0] = input_memcpy_buffer2[input_mmcpy_offset[2]];
					input_array[2][1] = input_memcpy_buffer2[input_mmcpy_offset[2]] >> 16;
					input_mmcpy_offset[2]++;
					NextInputFlag[2] = false;
				}

				if(NextInputFlag[3])
				{
					input_array[3][0] = input_memcpy_buffer3[input_mmcpy_offset[3]];
					input_array[3][1] = input_memcpy_buffer3[input_mmcpy_offset[3]] >> 16;
					input_mmcpy_offset[3]++;
					NextInputFlag[3] = false;
				}

				input_buffer[0][t2r][t3] = input_array[0][cnt[0]];
				input_buffer[1][t2r][t3] = input_array[1][cnt[1]];
				input_buffer[2][t2r][t3] = input_array[2][cnt[2]];
				input_buffer[3][t2r][t3] = input_array[3][cnt[3]];

				if(cnt[0]==1)
				{
					NextInputFlag[0] = true;
					cnt[0] = 0;
				}else
				{
					cnt[0] = 1;
				}

				if(cnt[1]==1)
				{
					NextInputFlag[1] = true;
					cnt[1] = 0;
				}else
				{
					cnt[1] = 1;
				}

				if(cnt[2]==1)
				{
					NextInputFlag[2] = true;
					cnt[2] = 0;
				}else
				{
					cnt[2] = 1;
				}

				if(cnt[3]==1)
				{
					NextInputFlag[3] = true;
					cnt[3] = 0;
				}else
				{
					cnt[3] = 1;
				}
			}else
			{
				input_buffer[0][t2r][t3] = pad_value;
				input_buffer[1][t2r][t3] = pad_value;
				input_buffer[2][t2r][t3] = pad_value;
				input_buffer[3][t2r][t3] = pad_value;
			}

		}

		t2_local += T2R;
}

void input_load(int *input,int *input1,int *input2,int *input3,
				short input_buffer[Tn][OnChipIB_Height][OnChipIB_Width],int r,int c,int n,int Kernel_stride,int Padding,UCHAR TRow,UCHAR TCol,int Input_w,int Input_h,int TN_MIN,int IHxIW,int LayerType,ap_uint<6> trow_loops)
{
	

}

void weight_mmcpy_everyKxK(int *Weight,int weight_memcpy_buffer[Tm*Tn/2],ap_uint<3> t3,ap_uint<3> t4,ap_uint<3> next_t3[1],ap_uint<3> next_t4[1],unsigned int ReadLength,bool init_enable,bool enable)
{
	
}

void load_weight2buf_everyKxK(int weight_memcpy_buffer[Tm*Tn/2],short weight_buffer[Tm][Tn][K][K],ap_uint<3> t3,ap_uint<3> t4,ap_uint<6> TM_MIN,ap_uint<3> TN_MIN,bool enable)
{

	
}

void weight_load_reorg(int *Weight,short weight_buffer[Tm][Tn][K][K],bool weight_load_enable,int m,int n,int IFM_numxKxK,int KxK,int Kernel_size,int TM_MIN,int TN_MIN)
{
	
}


void copy_input_weight(int *input,int *input1,int *input2,int *input3,int *Weight,int InFM_num,int Input_w,int Input_h,int Kernel_size,int Kernel_stride,int r,int c,int m,int n,
		int TM_MIN,int TN,UCHAR TRow,UCHAR TCol,int Padding,short input_buffer[Tn][OnChipIB_Height][OnChipIB_Width],short weight_buffer[Tm][Tn][K][K],int TMP_N_next[1],
		bool enable,bool weight_load_enable,bool initialize,const int IHxIW,const int KxK,const int IFM_numxKxK,const int LayerType,ap_uint<6> trow_loops)
{

}

//////////////////////////////////////////////////T3 end

void copy_local_beta(short beta_buffer[MAX_BETA_LENGTH],int local_beta_buffer[MAX_BETA_LENGTH],const int TM_MIN,int m,UCHAR InterSubBeta)
{
	
}

void compute(short input_buffer[Tn][OnChipIB_Height][OnChipIB_Width],int output_buffer[Tm][Tr][Tc],
		short weight_buffer[Tm][Tn][K][K],short beta_buffer[MAX_BETA_LENGTH],int TMP_N_next[1],
		const int Kernel_size,const int Kernel_stride,int TMP_M,
		const int TM_MIN,const int TR_MIN,const int TC_MIN,bool enable,const bool IsNL,const bool reluenable,
		UCHAR InterSubBeta,UCHAR WeightAddInputSubInter,UCHAR InterSubOutput)
{

	
}

//////////////version-0.2 start
void mmcpy_outputport(int *Output,int output_tmp[Tr*Tc/4],ap_uint<6> tm,ap_uint<6> mLoop,int OutputOffset,int OutputLength)
{
	bool enable = tm < mLoop;
	if(!enable)
		return;

	memcpy((int *)(Output + OutputOffset),(int *)(output_tmp),OutputLength*sizeof(int));
}

void mmcpy_outputport1(int *Output,int output_tmp[Tr*Tc/4],ap_uint<6> tm,ap_uint<6> mLoop,int OutputOffset,int OutputLength)
{
	bool enable = tm < mLoop;
	if(!enable)
		return;

	memcpy((int *)(Output + OutputOffset),(int *)(output_tmp),OutputLength*sizeof(int));
}

void mmcpy_outputpixel(int *Output,int *Output1,int output_tmp[Tr*Tc/4],int output_tmp1[Tr*Tc/4],ap_uint<6> tm,ap_uint<6> mLoop1,ap_uint<6> mLoop2,int outputoffsetarray[2],int OutputLength,int OutputLength1,bool enable)
{
	if(!enable)
	{
		return;
	}
	mmcpy_outputport(Output ,output_tmp ,tm,mLoop1,outputoffsetarray[0],OutputLength );
	mmcpy_outputport1(Output1,output_tmp1,tm,mLoop2,outputoffsetarray[1],OutputLength1);
}

void outputpixel2buf(int output_buffer[Tm][Tr][Tc],int output_tmp[Tr*Tc/4],int output_tmp1[Tr*Tc/4],bool IsNL,int InterSubOutput,int LayerType,bool TC_MINe26,int TR_MIN,int TC_MIN,int mLoop,int rLoop, bool init,
					 int outputoffsetarray[2],int OutputOffset1_sum,int OutputOffset1_sum1,int OutputOffset2_sum,ap_uint<6> tm_next[1],bool enable)
{
	

}

void write_back_output_reorg(int output_buffer[Tm][Tr][Tc],int *Output,int *Output1,int r,int c,int m,const int Output_w,const int Output_h,
					   const int TM_MIN,const int TR_MIN,const int TC_MIN,const int OHxOW,bool write_flag,const int OutputQ,bool IsNL,int InterSubOutput,int LayerType)
{
	

}

void pool_yolo2(short Input[Tn][OnChipIB_Height][OnChipIB_Width],int Output[Tm][Tr][Tc],
		  const int Kernel_size,const int Kernel_stride,
		  const int TM_MIN,const int TR_MIN,const int TC_MIN,bool enable)
{

}

void reorg_yolo2(short Input[Tn][OnChipIB_Height][OnChipIB_Width],int Output[Tm][Tr][Tc],
		  const int Kernel_size,const int Kernel_stride,
		  const int TM_MIN,const int TR_MIN,const int TC_MIN,bool enable)
{
	
}

void intra_pingpong_wrapper(int *Input,int *Input1,int *Input2,int *Input3,int *Weight, int output_buffer[Tm][Tr][Tc],short beta_buffer[MAX_BETA_LENGTH],
								 short input_buffer0[Tn][OnChipIB_Height][OnChipIB_Width],short input_buffer1[Tn][OnChipIB_Height][OnChipIB_Width],
								 int InFM_num,int Input_w,int Input_h,int Kernel_size,int Kernel_stride,
								 int TMP_R,int TMP_C,int TMP_M,int m,int TM_MIN,int TR_MIN,int TC_MIN,int TN,UCHAR TRow,UCHAR TCol,int Padding,
								 int IHxIW,int KxK,int IFM_numxKxK,int nLoops,bool IsNL,int LayerType,int TM,int TMP_X_next[1],int TX_MIN_next[1],bool pingpongx,bool input_flag,bool process_flag,
								 UCHAR InterSubBeta,UCHAR WeightAddInputSubInter,UCHAR InterSubOutput,ap_uint<6> trow_loops)
{

	

}

void copy_beta(short beta_buffer[MAX_BETA_LENGTH],int *Beta,const int OFM_NUM,const int BetaQ)
{
	
}

void YOLO2_FPGA(int *Input,int *Input1,int *Input2,int *Input3,int *Output,int *Output1,int *Weight,int *Beta,const int InFM_num,const int OutFM_num,
							  const int Kernel_size,const int Kernel_stride,
							  const int Input_w,const int Input_h,const int output_w,const int output_h,const int Padding,const bool IsNL,const bool IsBN,
							  const int TM,const int TN,const int TR,const int TC,
							  const int mLoops,const int nLoops,const int rLoops,const int cLoops,const int LayerType,
							  const int InputQ,const int OutputQ,const int WeightQ,const int BetaQ,int trow_loops)
{

#pragma HLS INTERFACE m_axi depth=512 port=Input   offset=slave bundle=DATA_BUS1 num_read_outstanding=1 num_write_outstanding=1 max_read_burst_length=64 max_write_burst_length=64
#pragma HLS INTERFACE m_axi depth=512 port=Input1  offset=slave bundle=DATA_BUS2 num_read_outstanding=1 num_write_outstanding=1 max_read_burst_length=64 max_write_burst_length=64
#pragma HLS INTERFACE m_axi depth=512 port=Input2  offset=slave bundle=DATA_BUS3 num_read_outstanding=1 max_read_burst_length=64
#pragma HLS INTERFACE m_axi depth=512 port=Input3  offset=slave bundle=DATA_BUS4 num_read_outstanding=1 max_read_burst_length=64
#pragma HLS INTERFACE m_axi depth=512 port=Output  offset=slave bundle=DATA_BUS1 num_read_outstanding=1 num_write_outstanding=1 max_read_burst_length=64 max_write_burst_length=64
#pragma HLS INTERFACE m_axi depth=512 port=Output1 offset=slave bundle=DATA_BUS2 num_read_outstanding=1 num_write_outstanding=1 max_read_burst_length=64 max_write_burst_length=64
#pragma HLS INTERFACE m_axi depth=512 port=Weight  offset=slave bundle=DATA_BUS5 num_read_outstanding=1 max_read_burst_length=128
#pragma HLS INTERFACE m_axi depth=512 port=Beta    offset=slave bundle=DATA_BUS5 num_read_outstanding=1 max_read_burst_length=128

#pragma HLS INTERFACE s_axilite register port=return bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=InFM_num bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=OutFM_num bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=Kernel_size bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=Kernel_stride bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=Input_w bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=Input_h bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=output_w bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=output_h bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=Padding bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=IsNL bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=IsBN bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=TM bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=TN bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=TR bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=TC bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=mLoops bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=nLoops bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=rLoops bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=cLoops bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=LayerType bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=InputQ bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=OutputQ bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=WeightQ bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=BetaQ bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=trow_loops bundle=CTRL_BUS

#pragma HLS INTERFACE s_axilite register port=Input bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=Output bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=Weight bundle=CTRL_BUS
#pragma HLS INTERFACE s_axilite register port=Beta bundle=CTRL_BUS

	
}
////////////////////////////////////////////20181229 n4m32  v2 without input and reorg opt end input opt ok relu comb ok // input opt ok //output opt ok //weight opt ok (5)n4m32i4o2 ok end
