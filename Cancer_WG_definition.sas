/* Working Group cancer of interest */
Anyc_Tumo = 0 ; Uadt_Tumo = 0 ; Stom_Tumo = 0 ; Sint_Tumo = 0 ; Clrt_Tumo = 0 ; Anog_Tumo = 0 ; Live_Tumo = 0 ; Panc_Tumo = 0 ;
Lung_Tumo = 0 ; Meso_Tumo = 0 ; Skin_Tumo = 0 ; Ovar_Tumo = 0 ; Brea_Tumo = 0 ; Ceru_Tumo = 0 ; Coru_Tumo = 0 ; Pros_Tumo = 0 ;
Kidn_Tumo = 0 ; Blad_Tumo = 0 ; Brai_Tumo = 0 ; Thyr_Tumo = 0 ; Cups_Tumo = 0 ; Lymp_Tumo = 0 ; Leuk_Tumo = 0 ; Rare_Tumo = 0 ;

If Not (Substr(Sit_Tumo,1,3) eq Upcase('C44') and Substr(Mor_Tumo_Icdo3,1,3) in ('809','810','811')) Then Anyc_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C00','C01','C02','C03','C04','C05','C06','C07',
														 'C09','C10','C11','C12','C13','C14','C15','C32')) Then Uadt_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C15','C16')) Then Stom_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C17')) Then Sint_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C18','C19','C20')) Then Clrt_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C21','C51','C52','C53','C60')) Then Anog_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C22','C23','C24')) Then Live_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C25')) Then Panc_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C34')) Then Lung_Tumo = 1 ;

If (Sit_Tumo eq 'C384' and Substr(Mor_Tumo_Icdo3,1,3) in ('905')) Then Meso_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C44')) or
	 (Substr(Mor_Tumo_Icdo3,1,3) in ('872','873','874','875','876','877','878','879')) Then Skin_Tumo = 1 ;

If ((Substr(Sit_Tumo,1,3) in ('C48','C56') or Sit_Tumo eq 'C570') and Sex eq 2) Then Ovar_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C50')) Then Brea_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C53') and Sex eq 2) Then Ceru_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C54') and Sex eq 2) Then Coru_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C61') and Sex eq 1) Then Pros_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C64','C65')) Then Kidn_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C67')) Then Blad_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C70','C71','C72')) Then Brai_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C73')) Then Thyr_Tumo = 1 ;

If (Substr(Sit_Tumo,1,3) in ('C80')) Then Cups_Tumo = 1 ;

If (Cncr_Lymphoma eq 1) Then Lymp_Tumo = 1 ;

If (Substr(Mor_Tumo_Icdo3,1,3) in ('970','973','976',
															'980','981','982','983','984','985','986','987','988','989',
															'990','991','992','993','994','995','996','998')) Then Leuk_Tumo = 1 ;
/* Rare_Tumo : See specific program */
Label
	Anyc_Tumo = 'Any tumour'
	Uadt_Tumo = 'Upper aero-digestive tract tumour'
	Stom_Tumo = 'Stomach/Esophagus tumour'
	Sint_Tumo = 'Small intestine tumour'
	Clrt_Tumo = 'Colon-rectum tumour'
	Anog_Tumo = 'Anogenital tumour'
	Live_Tumo = 'Liver tumour'
	Panc_Tumo = 'Pancreas tumour'
	Lung_Tumo = 'Lung tumour'
	Meso_Tumo = 'Mesothelioma tumour'
	Skin_Tumo = 'Skin/Melanoma tumour'
	Ovar_Tumo = 'Ovary tumour'
	Brea_Tumo = 'Breast tumour'
	Ceru_Tumo = 'Cervix uteri tumour'
	Coru_Tumo = 'Corpus uteri tumour'
	Pros_Tumo = 'Prostate tumour'
	Kidn_Tumo = 'Kidney tumour'
	Blad_Tumo = 'Bladder tumour'
	Brai_Tumo = 'Brain tumour'
	Thyr_Tumo = 'Thyroid tumour'
	Cups_Tumo = 'Unknown primary site tumour'
	Lymp_Tumo = 'Lymphoma'
	Leuk_Tumo = 'Leukemia tumour'
	Rare_Tumo = 'Rare tumour' ;
Format
	Anyc_Tumo Uadt_Tumo Stom_Tumo Sint_Tumo Clrt_Tumo Anog_Tumo Live_Tumo Panc_Tumo
	Lung_Tumo Meso_Tumo Skin_Tumo Ovar_Tumo Brea_Tumo Ceru_Tumo Coru_Tumo Pros_Tumo
	Kidn_Tumo Blad_Tumo Brai_Tumo Thyr_Tumo Cups_Tumo Lymp_Tumo Leuk_Tumo Rare_Tumo Yes_No. ;

/* Working Group specific variables */
/* Breast */
Length Brea_Typ_Tumo $40. Brea_Excl_Tumo $4. ;
If Brea_Tumo eq 1 Then Do ;
	If Mor_Tumo_Icdo3 in ('8000/3','8001/3','8010/3','8012/3','8020/3','8021/3','8022/3','8032/3','8033/3','8046/3','8050/3','8070/3','8071/3',
		'8076/3','8140/3','8141/3','8190/3','8200/3','8201/3','8211/3','8230/3','8246/3','8255/3','8260/3','8315/3','8323/3','8341/3','8345/3',
		'8401/3','8453/3','8480/3','8480/3','8481/3','8490/3','8500/3','8501/3','8502/3','8503/3','8504/3','8507/3','8510/3','8512/3','8513/3',
		'8520/3','8521/3','8522/3','8523/3','8524/3','8530/3','8541/3','8550/3','8560/3','8570/3','8572/3','8573/3','8574/3','8575/3') Then Do ;
		Brea_Typ_Tumo = '01-Breast invasive epithelial' ; Brea_Excl_Tumo = 'Incl' ; End ;
	Else If Mor_Tumo_Icdo3 in ('8000/2','8010/2','8050/2','8140/2','8201/2','8230/2','8260/2','8401/2','8500/2','8501/2','8503/2','8504/2',
				'8507/2','8520/2','8522/2','8523/2','8540/2','8543/2','8741/2','8540/3','8543/3') Then Do ;
				Brea_Typ_Tumo = '02-Breast in-situ' ; Brea_Excl_Tumo = 'Incl' ; End ;
	Else If Mor_Tumo_Icdo3 in ('8000/0','8000/1','8001/1','8010/0','8040/1','8050/0','8140/','8140/0','8140/1','8480/','8500/','8501/','8503/',
				'8503/0','8504/0','8510/','8520/','8521/','9010/0','9020/0','9020/1') Then Do ;
				Brea_Typ_Tumo = '08-Breast ineligible' ; Brea_Excl_Tumo = 'Excl' ; End ;
	Else If Mor_Tumo_Icdo3 in ('8013/3','8090/3','8143/3','8144/3','8210/3','8231/3','8562/3','8580/3','8586/3','8800/3','8801/3','8804/3',
				'8810/3','8832/3','8890/3','8980/3','8982/3','9020/3','9120/3','9180/3','9590/3','9675/3','9680/3','9690/3','9691/3','9699/3') Then Do ;
				Brea_Typ_Tumo = '09-Breast excluded' ; Brea_Excl_Tumo = 'Excl' ; End ;
	Else If Mor_Tumo_Icdo3 eq '' Then Do ;
				Brea_Typ_Tumo = '99-Breast missing' ; Brea_Excl_Tumo = 'Excl' ; End ;
End ;
Label Brea_Typ_Tumo = 'Classification of breast tumour'
			Brea_Excl_Tumo = 'Included/Excluded breast tumour' ;

/* Colorectal */
Length Clrt_Typ_Tumo $40. Clrt_Excl_Tumo $4. ;
If Clrt_Tumo eq 1 Then Do ;
	If Mor_Tumo_Icdo3 in ('8010/3','8020/3','8140/3','8143/3','8144/3','8201/3','8210/3','8211/3','8221/3','8260/3','8261/3','8262/3','8263/3',
		'8480/3','8481/3','8490/3','8510/3','8560/3') Then Do ;
		Clrt_Typ_Tumo = '01-Colorectal malignant' ; Clrt_Excl_Tumo = 'Incl' ; End ;
	Else If Mor_Tumo_Icdo3 in ('8101/2','8140/2','8144/2','8210/2','8261/2','8263/2') Then Do ;
				Clrt_Typ_Tumo = '02-Colorectal in-situ' ; Clrt_Excl_Tumo = 'Incl' ; End ;
	Else If Mor_Tumo_Icdo3 in ('8000/0','8000/1','8000/9','8090/3','8140/','8140/1','8140/6','8210/','8211/0','8261/','8261/0','8263/1',
				'8440/3','8470/3','8472/1','8480/','8480/6','8500/3','8950/3','9675/') Then Do ;
				Clrt_Typ_Tumo = '08-Colorectal ineligible' ; Clrt_Excl_Tumo = 'Excl' ; End ;
	Else If Mor_Tumo_Icdo3 in ('8000/3','8001/3','8003/3','8010/2','8012/3','8021/3','8022/3','8041/3','8045/3','8070/2','8070/3','8083/3',
				'8123/3','8141/3','8190/3','8200/3','8230/3','8240/2','8240/3','8241/3','8243/3','8244/3','8245/3','8246/3','8255/3','8323/3',
				'8345/3','8472/3','8511/3','8574/3','8720/3','8890/3','8936/3','8990/3','9590/3','9671/3','9680/3','9699/3','9714/3','9823/3') Then Do ;
				Clrt_Typ_Tumo = '09-Colorectal excluded' ; Clrt_Excl_Tumo = 'Excl' ; End ;
	Else If Mor_Tumo_Icdo3 eq '' Then Do ;
				Clrt_Typ_Tumo = '99-Colorectal missing' ; Clrt_Excl_Tumo = 'Excl' ; End ;

			Case_In_Situ = . ;
			Case_Mal_Colon_Prox = . ;
			Case_Mal_Colon_Dist = . ;
			Case_Mal_Colon_Nos = . ;
			Case_Mal_Rectum = . ;
			If Beh_Tumo eq 2 Then Case_In_Situ = 1 ;
			if Clrt_Typ_Tumo = '01-Colorectal malignant' Then Do ;
				If Sit_Tumo in ('C180','C181','C182','C183','C184','C185') Then Case_Mal_Colon_Prox = 1 ;
				If Sit_Tumo in ('C186','C187') Then Case_Mal_Colon_Dist = 1 ;
				If Sit_Tumo in ('C188','C189') Then Case_Mal_Colon_Nos = 1 ;
				If Sit_Tumo in ('C199','C209') Then Case_Mal_Rectum = 1 ;
			End ;
	End ;

	Label 		Clrt_Typ_Tumo = 'Classification of colorectal tumour'
				Clrt_Excl_Tumo = 'Included/Excluded colorectal tumour' 
				Case_In_Situ = 'In-situ tumour'
				Case_Mal_Colon_Prox = 'Malignant Colon Proximal'
				Case_Mal_Colon_Dist = 'Malignant Colon Distal'
				Case_Mal_Colon_Nos = 'Malignant Colon Overlapping/Nos'
				Case_Mal_Rectum = 'Malignant Rectum' ;
	Format Case_In_Situ Case_Mal_Colon_Prox Case_Mal_Colon_Dist Case_Mal_Colon_Nos Case_Mal_Rectum Yes_No. ;



/* Endometrium */
Length Coru_Typ_Tumo $40. Coru_Excl_Tumo $4. ;
If Coru_Tumo eq 1 Then Do ;
	If Mor_Tumo_Icdo3 in ('8380/2','8380/3','8382/3','8560/3','8570/3') Then Do ;
		Coru_Typ_Tumo = '01-Endometrioid tumour' ; Coru_Excl_Tumo = 'Incl' ; End ;
	Else If Mor_Tumo_Icdo3 in ('8310/3','8322/3','8441/3','8460/3') Then Do ;
				Coru_Typ_Tumo = '02-Serous/Clear cell tumour' ; Coru_Excl_Tumo = 'Incl' ; End ;
	Else If Mor_Tumo_Icdo3 in ('8000/3','8001/3','8010/3','8020/3','8022/3','8031/3','8050/3','8070/3','8140/3','8143/3','8144/3','8210/3',
				'8211/3','8255/3','8260/3','8323/3','8340/3','8450/3','8480/3','8481/3','8895/3','8950/3','8951/3','8980/3') Then Do ;
				Coru_Typ_Tumo = '07-Endometrial unclassified' ; Coru_Excl_Tumo = 'Incl' ; End ;
	Else If Mor_Tumo_Icdo3 in ('8010/0','8010/2','8130/2','8140/','8140/1','8140/2','8140/9','8246/3','8260/','8263/3','8461/3','8503/3',
				'8560/','8570/','8572/3','8800/3','8890/3','8896/3','8930/','8930/3','8931/3','8933/3','8935/3','9100/3') Then Do ;
				Coru_Typ_Tumo = '09-Endometrial excluded' ; Coru_Excl_Tumo = 'Excl' ; End ;
	Else If Mor_Tumo_Icdo3 eq '' Then Do ;
				Coru_Typ_Tumo = '99-Endometrial missing' ; Coru_Excl_Tumo = 'Incl' ; End ;
End ;
Label Coru_Typ_Tumo = 'Classification of endometrial tumour'
			Coru_Excl_Tumo = 'Included/Excluded endometrial tumour' ;

/* Kidney */
Length Kidn_Typ_Tumo $40. ;
If Kidn_Tumo eq 1 Then Do ;
	If Site3 in ('C64') Then Do ;
		If Mor_Tumo_Icdo3 in ('8012/3','8021/3','8033/3','8050/3','8130/3','8140/3','8211/3','8246/3','8260/3','8270/3','8290/3',
			'8310/3','8312/3','8317/3','8318/3','8320/3','8322/3','8323/3','8500/3','8560/3') Then Kidn_Typ_Tumo = '01-Renal cell carcinoma' ;
		Else If Mor_Tumo_Icdo3 in ('8800/3','8830/3','8890/3','8964/3') Then Kidn_Typ_Tumo = '02-Kidney others' ;
		Else If Mor_Tumo_Icdo3 in ('8120/3') Then Kidn_Typ_Tumo = '03-Renal pelvis' ;
		Else If Mor_Tumo_Icdo3 eq '' Then Kidn_Typ_Tumo = '99-Kidney missing' ;
		Else Kidn_Typ_Tumo = '07-Kidney unclassified' ;
		End ;
	Else If Site3 in ('C65') Then Kidn_Typ_Tumo = '03-Renal pelvis' ;
End ;
Label Kidn_Typ_Tumo = 'Classification of kidney tumour' ;

/* Lymphoma */
Length Lymp_Typ_Tumo_1 Lymp_Typ_Tumo_2 Lymp_Typ_Tumo_3 $40. ;
If Lymp_Tumo eq 1 Then Do ;
	Lymp_Typ_Tumo_1 = 'Missing' ; Lymp_Typ_Tumo_2 = 'Missing' ; Lymp_Typ_Tumo_3 = 'Missing' ;
	If Cncr_Lymphoma_Malignant eq 1 Then Do ;
		If Mor_Tumo_Icdo3 eq '9590/3' Then Do ; Lymp_Typ_Tumo_1 = 'Malignant lymphoma' ; Lymp_Typ_Tumo_2 = 'Malignant lymphoma' ; Lymp_Typ_Tumo_3 = 'Malignant lymphoma' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9591/3' Then Do ; Lymp_Typ_Tumo_1 = 'Malignant lymphoma' ; Lymp_Typ_Tumo_2 = 'Malignant lymphoma' ; Lymp_Typ_Tumo_3 = 'Malignant lymphoma' ; End ;
		End ;
	Else If Cncr_Lymphoma_Hodgkin eq 1 Then Do ;
		If Mor_Tumo_Icdo3 eq '9650/3' Then Do ; Lymp_Typ_Tumo_1 = 'Hodgkin lymphoma' ; Lymp_Typ_Tumo_2 = 'Classical' ; Lymp_Typ_Tumo_3 = 'CHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9651/3' Then Do ; Lymp_Typ_Tumo_1 = 'Hodgkin lymphoma' ; Lymp_Typ_Tumo_2 = 'Classical' ; Lymp_Typ_Tumo_3 = 'LRCHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9652/3' Then Do ; Lymp_Typ_Tumo_1 = 'Hodgkin lymphoma' ; Lymp_Typ_Tumo_2 = 'Classical' ; Lymp_Typ_Tumo_3 = 'MCCHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9653/3' Then Do ; Lymp_Typ_Tumo_1 = 'Hodgkin lymphoma' ; Lymp_Typ_Tumo_2 = 'Classical' ; Lymp_Typ_Tumo_3 = 'LDCHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9659/3' Then Do ; Lymp_Typ_Tumo_1 = 'Hodgkin lymphoma' ; Lymp_Typ_Tumo_2 = 'NLPHL' ; Lymp_Typ_Tumo_3 = 'NLPHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9663/3' Then Do ; Lymp_Typ_Tumo_1 = 'Hodgkin lymphoma' ; Lymp_Typ_Tumo_2 = 'Classical' ; Lymp_Typ_Tumo_3 = 'NSCHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9664/3' Then Do ; Lymp_Typ_Tumo_1 = 'Hodgkin lymphoma' ; Lymp_Typ_Tumo_2 = 'Classical' ; Lymp_Typ_Tumo_3 = 'NSCHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9665/3' Then Do ; Lymp_Typ_Tumo_1 = 'Hodgkin lymphoma' ; Lymp_Typ_Tumo_2 = 'Classical' ; Lymp_Typ_Tumo_3 = 'NSCHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9667/3' Then Do ; Lymp_Typ_Tumo_1 = 'Hodgkin lymphoma' ; Lymp_Typ_Tumo_2 = 'Classical' ; Lymp_Typ_Tumo_3 = 'NSCHL' ; End ;
		End ;
	Else If Cncr_Lymphoma_Non_Hodgkin eq 1 Then Do ;
		If Mor_Tumo_Icdo3 eq '9670/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'CLL/SLL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9671/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'LPL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9673/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'MCL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9675/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'NHL' ; Lymp_Typ_Tumo_3 = 'NHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9678/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'PEL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9679/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'DLBCL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9680/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'DLBCL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9684/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'DLBCL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9687/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'BL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9689/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'MZL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9690/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'FL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9691/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'FL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9695/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'FL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9698/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'FL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9699/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'MZL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9700/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature T- or NK-NHL' ; Lymp_Typ_Tumo_3 = 'MF/SS' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9701/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature T- or NK-NHL' ; Lymp_Typ_Tumo_3 = 'MF/SS' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9702/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature T- or NK-NHL' ; Lymp_Typ_Tumo_3 = 'PTCL ' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9705/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature T- or NK-NHL' ; Lymp_Typ_Tumo_3 = 'PTCL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9709/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature T- or NK-NHL' ; Lymp_Typ_Tumo_3 = 'PTCL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9714/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature T- or NK-NHL' ; Lymp_Typ_Tumo_3 = 'ALCL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9717/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature T- or NK-NHL' ; Lymp_Typ_Tumo_3 = 'PTCL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9718/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature T- or NK-NHL' ; Lymp_Typ_Tumo_3 = 'Mature T- or NK-NHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9719/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature T- or NK-NHL' ; Lymp_Typ_Tumo_3 = 'Mature T- or NK-NHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9727/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Precursor NHL' ; Lymp_Typ_Tumo_3 = 'Precursor NHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9728/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Precursor NHL' ; Lymp_Typ_Tumo_3 = 'Precursor B-NHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9731/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'PCN/MM' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9732/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'PCN/MM' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9733/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'PCN/MM' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9734/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'PCN/MM' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9761/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'LPL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9820/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'NHL' ; Lymp_Typ_Tumo_3 = 'NHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9823/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'CLL/SLL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9826/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'BL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9827/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature T- or NK-NHL' ; Lymp_Typ_Tumo_3 = 'ATLL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9831/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature T- or NK-NHL' ; Lymp_Typ_Tumo_3 = 'Mature T- or NK-NHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9832/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'NHL' ; Lymp_Typ_Tumo_3 = 'NHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9833/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'B-PLL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9835/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Precursor NHL' ; Lymp_Typ_Tumo_3 = 'Precursor NHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9836/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Precursor NHL' ; Lymp_Typ_Tumo_3 = 'Precursor B-NHL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9837/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Precursor NHL' ; Lymp_Typ_Tumo_3 = 'T-ALL/LBL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9940/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature B-NHL' ; Lymp_Typ_Tumo_3 = 'HCL' ; End ;
		Else If Mor_Tumo_Icdo3 eq '9948/3' Then Do ; Lymp_Typ_Tumo_1 = 'NHL' ; Lymp_Typ_Tumo_2 = 'Mature T- or NK-NHL' ; Lymp_Typ_Tumo_3 = 'Mature T- or NK-NHL' ; End ;
	End ;
End ;
Label Lymp_Typ_Tumo_1 = 'Classification of lymphoma tumour'
			Lymp_Typ_Tumo_2 = 'Classification of lymphoma tumour (level 2)'
			Lymp_Typ_Tumo_3 = 'Classification of lymphoma tumour (level 3)' ;

/* Ovary */
Length Ovar_Typ_Tumo $40. Ovar_Excl_Tumo $4. ;
If Ovar_Tumo eq 1 Then Do ;
	If Sit_Tumo eq 'C569' Then Do ;
		If Mor_Tumo_Icdo3 in ('8050/3','8260/3','8441/1','8441/3','8442/1','8442/3','8450/3','8451/1','8451/3','8460/3','8461/3','8462/1',
			'8462/3','9014/1','9014/3') Then Do ;
			Ovar_Typ_Tumo = '01-Serous tumour' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('8470/1','8470/3','8471/3','8472/1','8472/3','8473/1','8473/3','8480/3','8481/3') Then Do ;
					Ovar_Typ_Tumo = '02-Mucinous tumour' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('8380/3','8381/1','8560/3','8570/3') Then Do ;
					Ovar_Typ_Tumo = '03-Endometrioid tumour' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('8310/3','8313/3','8322/3','9110/3') Then Do ;
					Ovar_Typ_Tumo = '04-Clear cell tumour' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('8010/2','8010/3','8140/3','8255/3','8440/3') Then Do ;
					Ovar_Typ_Tumo = '05-Not otherwise specified' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('8000/3','8012/3','8020/3','8021/3','8022/3','8033/3','8070/3','8120/3','8230/3','8323/3','8500/3',
					'8586/3','8935/3','8950/3','8951/3','8980/3','9000/1','9000/3','9061/3','9100/3') Then Do ;
					Ovar_Typ_Tumo = '07-Ovarian unclassified' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('8001/3','8022/9','8041/3','8042/3','8140/6','8170/3','8240/3','8243/3','8245/3','8246/3','8263/3',
					'8313/1','8380/','8380/1','8440/','8440/1','8440/2','8441/','8441/0','8450/9','8470/0','8480/','8481/9','8490/3',
					'8580/3','8600/3','8620/1','8620/3','8621/1','8650/3','8800/3','8810/3','8858/3','8890/3','8930/3','8936/3',
					'9015/1','9060/3','9071/3','9080/','9080/0','9080/1','9080/3','9084/3','9680/3') Then Do ;
					Ovar_Typ_Tumo = '09-Ovarian excluded' ; Ovar_Excl_Tumo = 'Excl' ; End ;
		Else If Mor_Tumo_Icdo3 eq '' Then Do ;
					Ovar_Typ_Tumo = '99-Ovarian missing' ; Ovar_Excl_Tumo = 'Excl' ; End ;

		If Mor_Tumo_Icdo3 in ('8381/1','8441/1','8442/1','8442/3','8451/1','8451/3','8462/1','8462/3','8470/1','8472/1','8472/3','8473/1',
			'8473/3','9000/1','9014/1','9015/1') Then Ovar_Borderline = 1 ;
		Else If Mor_Tumo_Icdo3 eq '' Then Ovar_Borderline = . ;
		Else Ovar_Borderline = 0 ;
		End ;
	Else If Site3 eq 'C48' Then Do ;
		If Mor_Tumo_Icdo3 in ('8260/3','8441/3','8460/3','8461/3') Then Do ; Ovar_Typ_Tumo = '01-Serous tumour' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('8470/3') Then Do ; Ovar_Typ_Tumo = '02-Mucinous tumour' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('xxxx/x') Then Do ; Ovar_Typ_Tumo = '03-Endometrioid tumour' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('xxxx/x') Then Do ; Ovar_Typ_Tumo = '04-Clear cell tumour' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('8140/3') Then Do ; Ovar_Typ_Tumo = '05-Not otherwise specified' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('8010/3','8230/3') Then Do ; Ovar_Typ_Tumo = '07-Ovarian unclassified' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('8000/3','8001/3','8240/3','8480/3','8680/3','8800/3','8802/3','8805/3','8822/1','8830/3','8850/3',
					'8851/3','8852/3','8854/3','8855/3','8858/3','8890/3','8936/3','8990/3','9040/3','9050/3','9053/3','9101/3','9120/3',
					'9150/3','9540/3','9591/3','9670/3','9680/3','9695/3','9823/3') Then Do ;
					Ovar_Typ_Tumo = '09-Ovarian excluded' ; Ovar_Excl_Tumo = 'Excl' ; End ;
		Else If Mor_Tumo_Icdo3 eq '' Then Do ; Ovar_Typ_Tumo = '99-Ovarian missing' ; Ovar_Excl_Tumo = 'Excl' ; End ;

		If Mor_Tumo_Icdo3 in ('xxxx/x') Then Ovar_Borderline = 1 ;
		Else If Mor_Tumo_Icdo3 eq '' Then Ovar_Borderline = . ;
		Else Ovar_Borderline = 0 ;
		End ;
	Else If Sit_Tumo eq 'C570' Then Do ;
		If Mor_Tumo_Icdo3 in ('8260/3','8441/3','8460/3','8461/3') Then Do ; Ovar_Typ_Tumo = '01-Serous tumour' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('xxxx/x') Then Do ; Ovar_Typ_Tumo = '02-Mucinous tumour' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('8380/3') Then Do ; Ovar_Typ_Tumo = '03-Endometrioid tumour' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('8310/3') Then Do ; Ovar_Typ_Tumo = '04-Clear cell tumour' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('8010/3','8140/3') Then Do ; Ovar_Typ_Tumo = '05-Not otherwise specified' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('8000/3','8020/3','8950/3','8951/3') Then Do ; Ovar_Typ_Tumo = '07-Ovarian unclassified' ; Ovar_Excl_Tumo = 'Incl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('8010/2','8032/3','8440/2') Then Do ; Ovar_Typ_Tumo = '09-Ovarian excluded' ; Ovar_Excl_Tumo = 'Excl' ; End ;
		Else If Mor_Tumo_Icdo3 in ('') Then Do ; Ovar_Typ_Tumo = '99-Ovarian missing' ; Ovar_Excl_Tumo = 'Excl' ; End ;

		If Mor_Tumo_Icdo3 in ('xxxx/x') Then Ovar_Borderline = 1 ;
		Else If Mor_Tumo_Icdo3 eq '' Then Ovar_Borderline = . ;
		Else Ovar_Borderline = 0 ;
	End ;
End ;
Label Ovar_Typ_Tumo   = 'Classification of ovarian tumour'
			Ovar_Excl_Tumo  = 'Included/Excluded ovarian tumour'
			Ovar_Borderline = 'Borderline ovarian tumour' ;
Format Ovar_Borderline Yes_No. ;

/* Prostate */
Length Pros_Typ_Tumo $40. ;
If Pros_Tumo eq 1 Then Do ;
	If Cncr_Prostate_Advanced eq 1 Then Pros_Typ_Tumo = '01-Advanced' ;
	Else If Cncr_Prostate_Localised eq 1 Then Pros_Typ_Tumo = '02-Localised' ;
	Else Pros_Typ_Tumo = '03-Others' ;
End ;
Label Pros_Typ_Tumo = 'Classification of prostate tumour' ;

/* Skin/Melanoma */
Length Skin_Typ_Tumo $40. ;
If Skin_Tumo eq 1 Then Do ;
	If Site3 in ('C44') Then Do ;
		If Cncr_Skin_Scc eq 1 Then Skin_Typ_Tumo = '01-Skin squamous cell carcinoma' ;
		Else If Cncr_Skin_Melanoma eq 1 Then Skin_Typ_Tumo = '03-Skin melanoma' ;
		Else If Substr(Mor_Tumo_Icdo3,1,3) in ('809','810','811') Then Skin_Typ_Tumo = '02-Skin basal cell carcinoma' ;
		Else If Mor_Tumo_Icdo3 ne '' Then Skin_Typ_Tumo = '07-Skin unclassified' ;
		Else Skin_Typ_Tumo = '99-Skin missing' ;
		End ;
	Else Skin_Typ_Tumo = '11-Melanoma other sites' ;
End ;
Label Skin_Typ_Tumo = 'Classification of skin/melanoma tumour' ;

/* Stomach/Esophagus */
Length Stom_Typ_Tumo $40. ;
If Stom_Tumo eq 1 Then Do ;
	If Site3 eq 'C15' Then Do ;
		If Beh_Tumo_Icdo3 eq 3 Then Do ;
			If Morpnum in (8070,8071,8072,8073,8074,8076,8078) Then Stom_Typ_Tumo = '11-Esophagus squamous cell carcinoma' ;
			Else If Morpnum in (8140,8144,8480,8481,8490) Then Stom_Typ_Tumo = '12-Esophagus adenocarcinoma' ;
			Else If Morpnum in (8010,8012,8020,8230,8240,8241,8560) Then Stom_Typ_Tumo = '13-Esophagus other carcinoma' ;
			Else If Morpnum in (8000,8041,8246,8170,8500,8720,8890,9591,9680) Then Stom_Typ_Tumo = '17-Esophagus unclassified' ;
		End ;
		Else If Mor_Tumo_Icdo3 in ('8000/9','8010/2','8140/2','8480/2','8936/0','8990/1','9591/','9675/') Then Stom_Typ_Tumo = '18-Esophagus ineligible' ;
		Else If Mor_Tumo_Icdo3 eq '' Then Stom_Typ_Tumo = '99-Esophagus missing' ;
	End ;
	Else If Site3 eq 'C16' Then Do ;
		If Beh_Tumo_Icdo3 eq 3 Then Do ;
			If Morpnum in (8140,8141,8142,8143,8144,8145,8211,8230,8260,8261,8263,8323,8480,8481,8490,8560) Then Stom_Typ_Tumo = '01-Gastric adenocarcinoma' ;
			Else If Morpnum in (8010,8020,8021,8070,8082) Then Stom_Typ_Tumo = '02-Gastric carcinoma' ;
			Else If Morpnum in (8240,8246,8041) Then Stom_Typ_Tumo = '03-Gastric endocrine tumour' ;
			Else If Morpnum in (9590,9591,9595,9663,9670,9671,9675,9676,9680,9699,9702,9711,9715,9823) Then Stom_Typ_Tumo = '04-Gastric lymphoma' ;
			Else If Morpnum in (8720,8800,8890,8891,8930,8935,8936,8940) Then Stom_Typ_Tumo = '05-Mesenchymal and secondary tumour' ;
			Else If Morpnum in (8000,8001,8004,8990,9650,9673,9684,9690) Then Stom_Typ_Tumo = '07-Gastric unclassified' ;
		End ;
		Else If Mor_Tumo_Icdo3 in ('8000/9','8010/2','8140/2','8936/0','8990/1','9591/','9675/') Then Stom_Typ_Tumo = '08-Gastric ineligible' ;
		Else If Mor_Tumo_Icdo3 eq '' Then Stom_Typ_Tumo = '99-Gastric missing' ;
	End ;
End ;
Label Stom_Typ_Tumo = 'Classification of stomach/esophagus tumour' ;

/* Thyroid */
Length Thyr_Typ_Tumo $40. Thyr_TNM $5. Thyr_T $3. ;
If Thyr_Tumo eq 1 Then Do ;
	If Cncr_Thyroid_Papillary eq 1 Then Thyr_Typ_Tumo = '01-Papillary' ;
	Else If Cncr_Thyroid_Follicular eq 1 Then Thyr_Typ_Tumo = '02-Follicular' ;
	Else If Morpnum in (8246,8345,8346,8510,8511,8512,8513,8680)Then Thyr_Typ_Tumo = '03-Medullary' ;
	Else If (Morpnum ge 8020 and Morpnum le 8035) or Morpnum in (8190,8337) Then Thyr_Typ_Tumo = '04-Anaplastic' ;
	Else If Morpnum eq 9680 Then Thyr_Typ_Tumo = '05-Thyroid lymphoma' ;
	Else If (Morpnum ge 8000 and Morpnum le 8010) or Morpnum in (8140) Then Thyr_Typ_Tumo = '06-Not otherwise specified' ;
	Else If Morpnum eq . Then Thyr_Typ_Tumo = '99-Thyroid missing' ;
	Else Thyr_Typ_Tumo = '07-Thyroid unclassified' ;

	/* Recode TNM */
	If Substr(Tnm_Tumo,2,2) in ('T1','T2') Then Thyr_TNM = 'T1-T2' ;
	If Substr(Tnm_Tumo,2,2) in ('T3','T4') Then Thyr_TNM = 'T3-T4' ;
	If Substr(Tnm_Tumo,1,3) eq 'PT1' Then Thyr_T = 'T1' ;
	Else If Substr(Tnm_Tumo,1,3) eq 'PT2' Then Thyr_T = 'T2' ;
	Else If Substr(Tnm_Tumo,1,3) eq 'PT3' Then Thyr_T = 'T3' ;
	Else If Substr(Tnm_Tumo,1,3) eq 'PT4' Then Thyr_T = 'T4' ;
	Else If Substr(Tnm_Tumo,1,3) in ('PTX','PT ') Then Thyr_T = 'TX' ;
	Else If Substr(Tnm_Tumo,1,3) eq 'CT1' Then Thyr_T = 'CT1' ;
	Else If Substr(Tnm_Tumo,1,3) eq 'CT2' Then Thyr_T = 'CT2' ;
	Else If Substr(Tnm_Tumo,1,3) eq 'CT3' Then Thyr_T = 'CT3' ;
	Else If Substr(Tnm_Tumo,1,3) eq 'CT4' Then Thyr_T = 'CT4' ;
	Else If Substr(Tnm_Tumo,1,3) in ('CTX','CT ') Then Thyr_T = 'CTX' ;
End ;
Label Thyr_Typ_Tumo = 'Classification of thyroid tumour'
			Thyr_TNM = 'T1-T2/T3-T4 (TNM) classification'
			Thyr_T = 'T (TNM) classification' ;

/* Uadt */
Length Uadt_Typ_Tumo $40. ;
If Uadt_Tumo eq 1 Then Do ;
	If S_Dg_Tumo_1 in (1,2,3,4,5,11) Then Uadt_Typ_Tumo = '98-Self-reported' ;
	Else Do ;
		If Mor_Tumo_Icdo3 in ('8070/3','8071/3','8072/3','8073/3','8074/3','8076/3','8078/3') Then Do ;
			If Site3 eq 'C15' Then Uadt_Typ_Tumo = '11-Esophagus squamous cell carcinoma' ;
			Else Uadt_Typ_Tumo = '01-UADT squamous cell carcinoma' ;
			End ;
		Else If Mor_Tumo_Icdo3 in ('8000/3','8010/2','8010/3','8011/3','8012/3','8020/3','8032/3','8033/3','8041/3','8051/3',
					'8070/2','8082/3','8083/3','8090/3','8094/3','8120/3','8123/3','8140/2','8140/3','8144/3','8170/3','8200/3',
					'8201/3','8230/3','8246/3','8260/3','8430/3','8440/3','8480/2','8480/3','8481/3','8490/3','8500/3','8525/3',
					'8550/3','8560/3','8562/3','8720/3','8810/3','8850/3','8800/3','8810/3','8850/3','8940/3','8941/3','9120/3',
					'9220/3','9364/3','9591/3','9673/3','9675/3','9680/3','9690/3','9691/3','9699/3','9731/3','9734/3') Then Do ;
			If Site3 eq 'C15' Then Uadt_Typ_Tumo = '12-Esophagus others' ;
			Else Uadt_Typ_Tumo = '02-UADT others' ;
			End ;
		Else If Mor_Tumo_Icdo3 in ('8052/2','8071/2','8561/0','8940/0','9560/0','9683/') Then Do ;
			If Site3 eq 'C15' Then Uadt_Typ_Tumo = '18-Esophagus ineligible' ;
			Else Uadt_Typ_Tumo = '08-UADT ineligible' ;
			End ;
		Else If Mor_Tumo_Icdo3 eq '' Then Do ;
			If Site3 eq 'C15' Then Uadt_Typ_Tumo = '99-Esophagus missing' ;
			Else Uadt_Typ_Tumo = '99-UADT missing' ;
			End ;
		Else Do ;
			If Site3 eq 'C15' Then Uadt_Typ_Tumo = '17-Esophagus unclassified' ;
			Else Uadt_Typ_Tumo = '07-UADT unclassified' ;
			End ;
	End ;
End ;
Label Uadt_Typ_Tumo = 'Classification of uadt tumour' ;

/* liver*/

Length Live_Typ_Tumo $40. Case_HCC  Case_HCC_Wide 	Case_IBD 	Case_GBT 	Case_EBD 	Case_Gallblad 	Case_AOV 	Case_CCA_Intra 	Case_CCA_Extra 	Case_CCA_Extra_Peri Case_CCA_Extra_Dist 8.;

	If Live_Tumo eq 1 Then Do ;
		/* Case_HCC: Hepatocellular carcinoma (definite) */
		If Sit_Tumo eq 'C220' and Mor_Tumo_Icdo3 in ('8170/3','8171/3','8180/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_HCC = 1 ;
		If Sit_Tumo eq 'C221' and Mor_Tumo_Icdo3 in ('8170/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_HCC = 1 ;
		/* Case_HCC_Wide : Hepatocellular carcinoma (definite) plus other cases that probably are HCC */
		If Case_HCC eq 1 Then Case_HCC_Wide = 1 ;
		If Sit_Tumo eq 'C220' and Mor_Tumo_Icdo3 in ('8000/3','8010/3') and B_Dg_Tumo_1 in (20,25,50,55,70) Then Case_HCC_Wide = 1 ;
		If Sit_Tumo eq 'C220' and Mor_Tumo_Icdo3 in ('8000/3','8140/3') and Lab_Afp ge 12 Then Case_HCC_Wide = 1 ;
		/* Case_IBD : Intrahepatic bile duct */
		If Sit_Tumo eq 'C220' and Mor_Tumo_Icdo3 in ('8160/3','8211/3','8481/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_IBD = 1 ;
		If Sit_Tumo eq 'C221' and Mor_Tumo_Icdo3 in ('8000/3','8160/3','8161/3','8211/3','8260/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_IBD = 1 ;
		/* Case_GBT : Gallbladder and biliary tract */
		If Sit_Tumo eq 'C221' and Mor_Tumo_Icdo3 in ('8162/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_GBT = 1 ;
		If Sit_Tumo eq 'C239' and Mor_Tumo_Icdo3 in ('8000/3') and B_Dg_Tumo_1 not in (.,27,53,54,56,60) Then Case_GBT = 1 ;
		If Sit_Tumo eq 'C239' and Mor_Tumo_Icdo3 in ('8010/2','8010/3','8020/3','8140/3','8160/3','8260/3','8480/3','8490/3','8560/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_GBT = 1 ;
		If Sit_Tumo eq 'C240' and Mor_Tumo_Icdo3 in ('8000/3','8010/3','8140/3','8160/3','8162/3','8260/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_GBT = 1 ;
		If Sit_Tumo eq 'C248' and Mor_Tumo_Icdo3 in ('8000/3','8160/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_GBT = 1 ;
		If Sit_Tumo eq 'C249' and Mor_Tumo_Icdo3 in ('8000/3','8001/3','8010/3','8140/3','8160/3','8162/3','8480/3','8481/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_GBT = 1 ;
		/* Case_EBD : Extrahepatic bile duct */
		If Sit_Tumo eq 'C240' and Mor_Tumo_Icdo3 in ('8000/3','8010/3','8140/3','8160/3','8162/3','8260/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_EBD = 1 ;
		/* Case_Gallblad : Gallbladder */
		If Sit_Tumo eq 'C239' and Mor_Tumo_Icdo3 in ('8000/3') and B_Dg_Tumo_1 not in (.,27,53,54,56,60) Then Case_Gallblad = 1 ;
		If Sit_Tumo eq 'C239' and Mor_Tumo_Icdo3 in ('8010/2','8010/3','8020/3','8140/3','8160/3','8260/3','8480/3','8490/3','8560/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_Gallblad = 1 ;
		/* Case_AOV : Ampulla of Vater */
		If Sit_Tumo eq 'C241' and Mor_Tumo_Icdo3 in ('8000/3','8010/3','8140/3','8260/3','8480/3','8481/3','8490/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_AOV = 1 ;
		/* Case_CCA_Intra : Intrahepatic cholangiocarcinomas */
		If Sit_Tumo eq 'C220' and Mor_Tumo_Icdo3 in ('8160/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_CCA_Intra = 1 ;
		If Sit_Tumo eq 'C221' and Mor_Tumo_Icdo3 in ('8160/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_CCA_Intra = 1 ;
		/* Case_CCA_Extra : Extrahepatic cholangiocarcinomas [sum of Klatskin tumours and distal tumours NOT including Ampulla of Vater */
		If Sit_Tumo eq 'C221' and Mor_Tumo_Icdo3 in ('8162/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_CCA_Extra = 1 ;
		If Sit_Tumo eq 'C239' and Mor_Tumo_Icdo3 in ('8160/3') and B_Dg_Tumo_1 not in (.,27,53,54,56,60) Then Case_CCA_Extra = 1 ;
		If Sit_Tumo eq 'C240' and Mor_Tumo_Icdo3 in ('8160/3','8162/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_CCA_Extra = 1 ;
		If Sit_Tumo eq 'C248' and Mor_Tumo_Icdo3 in ('8160/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_CCA_Extra = 1 ;
		If Sit_Tumo eq 'C249' and Mor_Tumo_Icdo3 in ('8160/3','8162/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_CCA_Extra = 1 ;
		/* Case_CCA_Extra_Peri : Perihilar extrahepatic cholangiocarcinomas */
		If Sit_Tumo eq 'C221' and Mor_Tumo_Icdo3 in ('8162/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_CCA_Extra_Peri = 1 ;
		If Sit_Tumo eq 'C240' and Mor_Tumo_Icdo3 in ('8162/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_CCA_Extra_Peri = 1 ;
		If Sit_Tumo eq 'C249' and Mor_Tumo_Icdo3 in ('8162/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_CCA_Extra_Peri = 1 ;
		/* Case_CCA_Extra_Dist : Distal extrahepatic cholangiocarcinomas */ 
		If Sit_Tumo eq 'C239' and Mor_Tumo_Icdo3 in ('8160/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_CCA_Extra_Dist = 1 ;
		If Sit_Tumo eq 'C240' and Mor_Tumo_Icdo3 in ('8160/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_CCA_Extra_Dist = 1 ;
		If Sit_Tumo eq 'C248' and Mor_Tumo_Icdo3 in ('8160/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_CCA_Extra_Dist = 1 ;
		If Sit_Tumo eq 'C249' and Mor_Tumo_Icdo3 in ('8160/3') and B_Dg_Tumo_1 not in (.,53,54,56,60) Then Case_CCA_Extra_Dist = 1 ;


		If Case_HCC eq 1 Then Live_Typ_Tumo = Compress(Live_Typ_Tumo||'HCC/') ;
		If Case_HCC_Wide eq 1 and Case_HCC in (0,.) Then Live_Typ_Tumo = Compress(Live_Typ_Tumo||'HCC_Wide/') ;
		If Case_IBD eq 1 Then Live_Typ_Tumo = Compress(Live_Typ_Tumo||'IBD/') ;
		If Case_GBT eq 1 Then Live_Typ_Tumo = Compress(Live_Typ_Tumo||'GBT/') ;
		If Case_EBD eq 1 Then Live_Typ_Tumo = Compress(Live_Typ_Tumo||'EBD/') ;
		If Case_Gallblad eq 1 Then Live_Typ_Tumo = Compress(Live_Typ_Tumo||'Gallblad/') ;
		If Case_AOV eq 1 Then Live_Typ_Tumo = Compress(Live_Typ_Tumo||'AOV/') ;
		If Case_CCA_Intra eq 1 or Case_CCA_Extra eq 1 Then Live_Typ_Tumo = Compress(Live_Typ_Tumo||'CCA/') ;
		If Live_Typ_Tumo eq '' Then Do ;
			If Mor_Tumo_Icdo3 eq '' Then Live_Typ_Tumo = 'Exc-Morphology missing' ;
			Else If B_Dg_Tumo_1 in (.,53,54,56,60) Then Live_Typ_Tumo = 'Exc-Basis diagnosis ineligible' ;
					 Else Live_Typ_Tumo = 'Exc-Morphology ineligible' ;
		End ;
	End ;

	Label Live_Typ_Tumo = 'Classification of Liver tumour'
				Case_HCC = 'Hepatocellular carcinoma'
				Case_HCC_Wide = 'Hepatocellular carcinoma (wider definition)'
				Case_IBD = 'Intrahepatic bile duct'
				Case_GBT = 'Gallbladder and biliary tract'
				Case_EBD = 'Extrahepatic bile duct'
				Case_Gallblad = 'Gallbladder'
				Case_AOV = 'Ampulla of Vater'
				Case_CCA_Intra = 'Intrahepatic cholangiocarcinoma'
				Case_CCA_Extra = 'Extrahepatic cholangiocarcinoma'
				Case_CCA_Extra_Peri = 'Perihilar extrahepatic cholangiocarcinoma'
				Case_CCA_Extra_Dist = 'Distal extrahepatic cholangiocarcinoma' ;
	Format Case_HCC Case_HCC_Wide Case_IBD Case_GBT Case_EBD Case_Gallblad Case_AOV Case_CCA_Intra Case_CCA_Extra Case_CCA_Extra_Peri Case_CCA_Extra_Dist Yes_No. ;


