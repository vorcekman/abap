*&---------------------------------------------------------------------*
*& Report ZTRM_DI_0001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztrm_di_0001.

TABLES: vtbfha.

TYPES: BEGIN OF gty_flow,
         bukrs        TYPE bukrs,
         rfha         TYPE vtbfha-rfha,
         rkondgr      TYPE vtbfhapo-rkondgr,
         kontrh       TYPE vtbfha-kontrh,
         rportb       TYPE vtbfha-rportb,
         dzterm       TYPE vtbfhapo-dzterm,
         dblfz        TYPE vtbfha-dblfz,
         delfz        TYPE vtbfha-delfz,
         sbktyp       TYPE vtbfhapo-sbktyp,
         sfhazba      TYPE vtbfhapo-sfhazba,
         rfhazu       TYPE vtbfhapo-rfhazu,
         sgsart       TYPE vtbfha-sgsart,
         sfhaart      TYPE vtbfha-sfhaart,
         khwkurs      TYPE vtbfhapo-khwkurs,
         ssign        TYPE vtbfhapo-ssign,
         wzbetr       TYPE vtbfhapo-wzbetr,
         bzbetr       TYPE vtbfhapo-bzbetr,
         bhwbetr      TYPE vtbfhapo-bhwbetr,
         dis_flowtype TYPE tpm_dis_flowtype,
         dmbtr_val    TYPE dmbtr,
         hwaer_val    TYPE waers,
       END OF gty_flow,
       gtt_flow TYPE TABLE OF gty_flow.

TYPES: BEGIN OF gty_ufrshesap,
         bukrs            TYPE bukrs,
         rfha             TYPE tb_rfha,
         rkondgr          TYPE vtbfhapo-rkondgr,
         kontrh           TYPE vtbfha-kontrh,
         rportb           TYPE vtbfha-rportb,
         dblfz            TYPE vtbfha-dblfz,
         delfz            TYPE vtbfha-delfz,
         sbktyp           TYPE vtbfhapo-sbktyp,
         sfhazba          TYPE vtbfhapo-sfhazba,
         rfhazu           TYPE vtbfhapo-rfhazu,
         sgsart           TYPE vtbfha-sgsart,
         sfhaart          TYPE vtbfha-sfhaart,
         khwkurs          TYPE vtbfhapo-khwkurs,
         ssign            TYPE vtbfhapo-ssign,
         kosul            TYPE char1,
         dzterm           TYPE tb_dzterm,
         BZBETR_p         TYPE tb_bzbetr,
         bzbetr_pb        TYPE tb_wzbetr,
         sbktyp_p         TYPE tb_sbktyp,
         BZBETR_i         TYPE tb_bzbetr,
         bzbetr_ib        TYPE tb_wzbetr,
         sbktyp_i         TYPE tb_sbktyp,
         total            TYPE tb_bzbetr,
         faiz             TYPE trff_type_dec4float,
         days_to_mat      TYPE i,
         amortised_cost   TYPE tb_bzbetr,
         amortised_cost_t TYPE tb_bzbetr,
       END OF gty_ufrshesap,
       gtt_ufrshesap TYPE TABLE OF gty_ufrshesap.

TYPES : BEGIN OF gty_ufrsson,
          bukrs                TYPE bukrs,
          rfha                 TYPE tb_rfha,
          rkondgr              TYPE vtbfhapo-rkondgr,
          kontrh               TYPE vtbfha-kontrh,
          rportb               TYPE vtbfha-rportb,
          dzterm               TYPE vtbfhapo-dzterm,
          dblfz                TYPE vtbfha-dblfz,
          delfz                TYPE vtbfha-delfz,
          sbktyp               TYPE vtbfhapo-sbktyp,
          sfhazba              TYPE vtbfhapo-sfhazba,
          rfhazu               TYPE vtbfhapo-rfhazu,
          sgsart               TYPE vtbfha-sgsart,
          sfhaart              TYPE vtbfha-sfhaart,
          khwkurs              TYPE vtbfhapo-khwkurs,
          ssign                TYPE vtbfhapo-ssign,
          T_total              TYPE tb_bzbetr,
          t_totalup            TYPE tb_bzbetr,
          total_amortised      TYPE tb_bzbetr,
          total_amortised_up   TYPE tb_bzbetr,
          irr_interest_accrual TYPE tb_bzbetr,
          wzbetrt              TYPE tb_wzbetr,
        END OF gty_ufrsson,
        gtt_ufrsson TYPE TABLE OF gty_ufrsson.

TYPES : BEGIN OF gty_ct0004,
          bukrs       TYPE ztrm_ct0004-bukrs,
          sgsart      TYPE ztrm_ct0004-sgsart,
          sfhaart     TYPE ztrm_ct0004-sfhaart,
          sbktyp      TYPE ztrm_ct0004-sbktyp,
          hkont_short TYPE ztrm_ct0004-hkont_short,
          hkont_long  TYPE ztrm_ct0004-hkont_long,
          umskz_short TYPE ztrm_ct0004-umskz_short,
          umskz_long  TYPE ztrm_ct0004-umskz_long,
        END OF gty_ct0004,
        gtt_ct0004 TYPE TABLE OF gty_ct0004.

TYPES : BEGIN OF gty_payments ,
          bukrs    TYPE bukrs,
          rfha     TYPE tb_rfha,
          payments TYPE fima_tab_vzzpaym,
        END OF gty_payments,
        gtt_payments TYPE TABLE OF gty_payments.



TYPES: BEGIN OF gty_deal,
         bukrs                TYPE bukrs,
         rfha                 TYPE tb_rfha,
         val_area             TYPE tpm_val_area,
         acc_principle        TYPE accounting_principle,
         rkondgr              TYPE vtbfhapo-rkondgr,
         rfhazu               TYPE vtbfhapo-rfhazu,
         dblfz                TYPE vtbfha-dblfz,
         delfz                TYPE vtbfha-delfz,
         sgsart               TYPE vtbfha-sgsart,
         sfhaart              TYPE vtbfha-sfhaart,
         sbktyp               TYPE vtbfhapo-sbktyp,
         kontrh               TYPE vtbfha-kontrh,
         rportb               TYPE vtbfha-rportb,
         wzbetr               TYPE vtbfhapo-wzbetr,
         bzbetr               TYPE vtbfhapo-bzbetr,
         bhwbetr              TYPE vtbfhapo-bhwbetr,
         bzbetr_short         TYPE vtbfhapo-bzbetr,
         bhwbetr_short        TYPE vtbfhapo-bhwbetr,
         bzbetr_short_current TYPE vtbfhapo-bzbetr,
         bzbetr_long          TYPE vtbfhapo-bzbetr,
         bhwbetr_long         TYPE vtbfhapo-bhwbetr,
       END OF gty_deal,
       gtt_deal TYPE TABLE OF gty_deal.

TYPES: BEGIN OF gty_out,
         check            TYPE char1,
         bukrs            TYPE bukrs,
         val_area         TYPE tpm_val_area,
         rfha             TYPE tb_rfha,
         rkondgr          TYPE vtbfhapo-rkondgr,
         kontrh           TYPE vtbfha-kontrh,
         rportb           TYPE vtbfha-rportb,
         kontrh_txt       TYPE but000-name_org1,
         keydat           TYPE budat,
         sgsart           TYPE vtbfha-sgsart,
         sfhaart          TYPE vtbfha-sfhaart,
         dblfz            TYPE vtbfha-dblfz,
         delfz            TYPE vtbfha-delfz,
         acc_principle    TYPE accounting_principle,
         wzbetr           TYPE waers,
         dmbtr_val        TYPE dmbtr,
         hwaer_val        TYPE waers,
         kkurs            TYPE vtbfhazu-kkurs,
         sbktyp           TYPE ztrm_de_sbktyp,
         sbktyp_txt       TYPE char15,
         bzbetr_cap       TYPE tb_bzbetr,
         bhwbetr_cap      TYPE tb_bzbetr,
         bzbetr_short     TYPE tb_bzbetr,
         bhwbetr_short    TYPE tb_bzbetr,
         bzbetr_posted    TYPE tb_bzbetr,
         bhwbetr_posted   TYPE tb_bzbetr,
         bzbetr_postable  TYPE tb_bzbetr,
         bhwbetr_postable TYPE tb_bzbetr,
         hkont_short      TYPE saknr,
         hkont_long       TYPE saknr,
         umskz_short      TYPE umskz,
         umskz_long       TYPE umskz,
         belnr            TYPE belnr_d,
         belnr_rev        TYPE belnr_d,
         budat            TYPE budat,
         budat_rev        TYPE budat,
         zzclass          TYPE ztrm_de_class,
       END OF gty_out,
       gtt_out TYPE TABLE OF gty_out.

TYPES: BEGIN OF gty_curtp,
         curtp LIKE t001a-curtp,
         waers LIKE t001-waers,
       END OF gty_curtp,
       gtt_curtp TYPE TABLE OF gty_curtp.

TYPES: BEGIN OF gty_mess,
         msgty        TYPE syst-msgty,
         msgno        TYPE syst-msgno,
         msgid        TYPE syst-msgid,
         bukrs        TYPE bkpf-bukrs,
         belnr        TYPE bkpf-belnr,
         gjahr        TYPE bkpf-gjahr,
         rfha         TYPE vtbfhapo-rfha,
         message(100) TYPE c,
       END OF gty_mess,
       gtt_mess TYPE TABLE OF gty_mess.

TYPES: BEGIN OF gty_kontrh,
         kontrh   TYPE but000-partner,
         bu_group TYPE but000-bu_group,
         bu_sort1 TYPE but000-bu_sort1,
       END OF gty_kontrh.

DATA: gt_kontrh TYPE TABLE OF gty_kontrh.

*CONSTANTS: gc_blart TYPE blart VALUE 'SA'.
CONSTANTS: gc_blart TYPE blart VALUE 'VR'.
CONSTANTS: gc_sbktyp_val TYPE tb_sbktyp VALUE '30'.

DATA gt_flow TYPE gtt_flow.
DATA gt_ufrs TYPE gtt_flow.
DATA gs_ufrs TYPE gty_flow.
DATA gt_out TYPE gtt_out.
DATA gt_deal TYPE gtt_deal.
DATA gt_db_posted TYPE TABLE OF ztrm_ct0003.
DATA gt_class_acc TYPE TABLE OF ztrm_ct0004.
*DATA gt_ct0013 TYPE HKONT.
DATA gt_ct0013    TYPE TABLE OF ztrm_ct0013.
DATA gs_class_cust TYPE ztrm_ct0005.
DATA gt_mess TYPE gtt_mess.
DATA gt_detail TYPE gtt_flow.
DATA gs_detail TYPE gty_flow.
DATA gt_detail_out TYPE gtt_flow.
DATA gt_class_log TYPE TABLE OF ztrm_ct0003.
DATA gs_transfer TYPE ztrm_ct0006.
DATA gs_db_val TYPE ztrm_ct0007.

DATA gv_budat_nextyear TYPE budat.
DATA gs_t001 TYPE t001.
DATA gs_t001a TYPE t001a.
DATA gt_curtp TYPE gtt_curtp.
DATA go_ref TYPE REF TO cl_gui_alv_grid.

DATA:lv_rfha  TYPE tb_rfha,
     lv_tarih TYPE datum.

DATA: gt_ufrshesap TYPE gtt_ufrshesap,
      gs_ufrshesap TYPE gty_ufrshesap,
      gt_ufrsson   TYPE gtt_ufrsson,
      gs_ufrsson   TYPE gty_ufrsson,
      gt_payments  TYPE fima_tab_vzzpaym,
      gs_payments  TYPE vzzpaym,
      gv_iff       TYPE trff_type_dec4float,
      gt_deep      TYPE gtt_payments,
      gv_days      TYPE char10,
      gv_datum     TYPE datum,
      gt_ct0004    TYPE gtt_ct0004,
      gs_ct0004    TYPE gty_ct0004.

SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME.
  SELECT-OPTIONS: s_bukrs FOR vtbfha-bukrs OBLIGATORY.
  SELECT-OPTIONS: s_rfha FOR vtbfha-rfha.
  SELECT-OPTIONS: s_rportb FOR vtbfha-rportb.
  SELECT-OPTIONS: s_kontrh FOR vtbfha-kontrh MATCHCODE OBJECT bupa..
  SELECT-OPTIONS: s_sgsart FOR vtbfha-sgsart.
  PARAMETERS: p_val TYPE tpm_val_area DEFAULT '001'. "OBLIGATORY DEFAULT '001' .
SELECTION-SCREEN: END OF BLOCK blk1.

SELECTION-SCREEN: BEGIN OF BLOCK blk2 WITH FRAME.
  PARAMETERS: p_keydat TYPE budat OBLIGATORY.
  PARAMETERS: p_post TYPE budat OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK blk2.

SELECTION-SCREEN: BEGIN OF BLOCK blk3 WITH FRAME.
  PARAMETERS: p_std RADIOBUTTON GROUP gr1,
              p_log RADIOBUTTON GROUP gr1.
SELECTION-SCREEN: END OF BLOCK blk3.

INITIALIZATION.
  PERFORM update_log.

START-OF-SELECTION.
  PERFORM main_flow.
*&---------------------------------------------------------------------*
*&      Form  MAIN_FLOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM main_flow .
  DATA lt_fcat TYPE lvc_t_fcat.
  DATA ls_mess TYPE gty_mess.

  CLEAR: gt_ct0013,
         gt_kontrh.

  IF p_std = 'X'.
    PERFORM initial_ops.
    PERFORM get_data.

    SORT gt_mess.
    READ TABLE gt_mess WITH KEY msgty = 'E' TRANSPORTING NO FIELDS BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      PERFORM display_message.
      RETURN.
    ENDIF.

    PERFORM process_data.
    PERFORM calculate_short_term_flows.
    PERFORM get_periodic_data.

    PERFORM prepare_fcat_summary USING lt_fcat.

    LOOP AT gt_mess INTO ls_mess.
      IF ls_mess-msgty = 'E'.
        PERFORM display_message.
        EXIT.
      ENDIF.
    ENDLOOP.

    PERFORM display_output USING lt_fcat.
  ELSEIF p_log = 'X'.
    PERFORM display_log.
  ENDIF.

ENDFORM. " MAIN_FLOW
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  DATA ls_mess TYPE gty_mess.

  SELECT SINGLE mandt,                                  "#EC CI_NOORDER
                bukrs,
                budat
    FROM ztrm_ct0006
    INTO @gs_transfer
   WHERE bukrs IN @s_bukrs.

  SELECT SINGLE mandt,                                  "#EC CI_NOORDER
                bukrs,
                norev_yearend
    FROM ztrm_ct0005
    INTO @gs_class_cust
   WHERE bukrs IN @s_bukrs.

  SELECT SINGLE mandt,                                  "#EC CI_NOORDER
                bukrs,
                xfeld
    FROM ztrm_ct0007
    INTO @gs_db_val
   WHERE bukrs IN @s_bukrs.

  SELECT        bukrs,
                sgsart,
                sfhaart,
                sbktyp,
                hkont_short,
                hkont_long,
                umskz_short,
                umskz_long
    FROM ztrm_ct0004
    INTO TABLE @gt_ct0004
    WHERE bukrs IN @s_bukrs
      AND sbktyp = '31'.

  SELECT z1~mandt,
         z1~rfha,
         z1~bukrs,
         z1~belnr,
         z1~gjahr,
         z1~sbktyp,
         z1~val_area,
         z1~hkont_long,
         z1~rportb,
         z1~keydat,
         z1~budat,
         z1~belnr_rev,
         z1~budat_rev,
         z1~waers,
         z1~wrbtr,
         z1~dmbtr
   INTO TABLE @gt_db_posted
   FROM ztrm_ct0003 AS z1
  INNER JOIN bkpf
     ON z1~bukrs EQ bkpf~bukrs
    AND z1~belnr EQ bkpf~belnr
    AND z1~gjahr EQ bkpf~gjahr
    AND bkpf~stblg EQ @space
  WHERE z1~bukrs  IN @s_bukrs
    AND z1~keydat LE @p_keydat
    AND z1~rportb IN @s_rportb.

  SELECT z1~mandt,
         z1~rfha,
         z1~bukrs,
         z1~belnr,
         z1~gjahr,
         z1~sbktyp,
         z1~val_area,
         z1~hkont_long,
         z1~rportb,
         z1~keydat,
         z1~budat,
         z1~belnr_rev,
         z1~budat_rev,
         z1~waers,
         z1~wrbtr,
         z1~dmbtr
   APPENDING TABLE @gt_db_posted
   FROM ztrm_ct0003 AS z1
  WHERE z1~bukrs  IN @s_bukrs
    AND z1~keydat LE @p_keydat
    AND z1~rportb IN @s_rportb
    AND z1~belnr  EQ 'A000000000'.

  SELECT  vtbfhapo~bukrs,
          vtbfhapo~rfha,
          vtbfhapo~rkondgr,
          vtbfha~kontrh,
          vtbfha~rportb,
          vtbfhapo~dzterm,
          vtbfhazu~dblfz,
          vtbfhazu~delfz,
          vtbfhapo~sbktyp,
          vtbfhapo~sfhazba,
          vtbfhapo~rfhazu,
          vtbfha~sgsart,
          vtbfha~sfhaart,
          vtbfhapo~khwkurs,
          vtbfhapo~ssign,
          vtbfhapo~wzbetr,
          vtbfhapo~bzbetr,
          vtbfhapo~bhwbetr
    INTO TABLE @gt_flow
    FROM vtbfha
   INNER JOIN vtbfhapo
      ON vtbfha~bukrs     EQ vtbfhapo~bukrs
     AND vtbfha~rfha      EQ vtbfhapo~rfha
     AND vtbfha~rfhazul   EQ vtbfhapo~rfhazu
   INNER JOIN vtbfhazu
      ON vtbfhazu~bukrs   EQ vtbfha~bukrs
     AND vtbfhazu~rfha    EQ vtbfha~rfha
     AND vtbfhazu~rfhazu  EQ vtbfha~rfhazul
   WHERE vtbfhapo~bukrs   IN @s_bukrs
     AND vtbfha~rfha      IN @s_rfha
     AND vtbfha~rportb    IN @s_rportb
     AND vtbfha~saktiv    EQ '0'
     AND vtbfhapo~sstornobwg EQ ''
     AND vtbfhapo~sbewebe IN ('0','1','2')
     AND vtbfhapo~ssprgrd NOT IN ('2','4','6')
     AND vtbfha~sgsart    IN @s_sgsart
     AND vtbfha~kontrh    IN @s_kontrh
     AND ( vtbfha~rcomvalcl EQ  '2' OR vtbfha~rcomvalcl EQ  '3' )
*    AND  vtbfha~zz_tterm = '400'   "" ertuge 17.08.2020 11:11:56 - revize edilecek "sinemy
     AND vtbfha~dblfz     LE @p_keydat
     AND vtbfhapo~rkondgr EQ ''
     AND ( vtbfha~sanlf   EQ '550' OR vtbfha~sanlf EQ '540' OR vtbfha~sanlf EQ '580').



  LOOP AT gt_ct0004 INTO DATA(ls_ct0004).
    LOOP AT gt_flow INTO DATA(ls_flows) WHERE bukrs = ls_ct0004-bukrs AND sgsart = ls_ct0004-sgsart AND sfhaart = ls_ct0004-sfhaart.
      gs_ufrs = ls_flows.
      APPEND gs_ufrs TO gt_ufrs.
    ENDLOOP.
  ENDLOOP.
  IF gt_ufrs IS NOT INITIAL.
    PERFORM ufrs_detay.
  ENDIF.


  SELECT mandt,
         bukrs,
         val_area,
         sgsart,
         sfhaart,
         dis_flowtype,
         acc_symbol,
         sbktyp,
         wzbetr,
         hkont_short,
         hkont_long,
         umskz_short,
         umskz_long
    FROM ztrm_ct0004
    INTO TABLE @gt_class_acc
   WHERE ( bukrs IN @s_bukrs OR bukrs = '' ).

  SORT gt_class_acc BY bukrs        DESCENDING
                       val_area     DESCENDING
                       sgsart       DESCENDING
                       sfhaart      DESCENDING
                       dis_flowtype DESCENDING
                       acc_symbol   DESCENDING
                       sbktyp       DESCENDING
                       wzbetr       DESCENDING.

  IF gt_class_acc IS NOT INITIAL.
    SELECT hkont
      FROM ztrm_ct0013
      INTO CORRESPONDING FIELDS OF TABLE @gt_ct0013
      FOR ALL ENTRIES IN @gt_class_acc
      WHERE ktopl EQ 'SGHP'
        AND ( hkont EQ @gt_class_acc-hkont_short
           OR hkont EQ @gt_class_acc-hkont_long ).

    SORT: gt_ct0013 BY hkont.
  ENDIF.

  IF gt_flow IS INITIAL.
    ls_mess-message = 'Ürün Bulunamadı'(026) ##TEXT_POOL.
    ls_mess-msgty = 'E'.
    APPEND ls_mess TO gt_mess.
  ELSE.
    DATA(lt_kontrh) = gt_flow.
    SORT: lt_kontrh BY kontrh.
    DELETE ADJACENT DUPLICATES FROM: lt_kontrh COMPARING kontrh.
    IF lt_kontrh IS NOT INITIAL.
      SELECT partner AS kontrh,
             bu_group,
             bu_sort1
        FROM but000
        INTO TABLE @gt_kontrh
        FOR ALL ENTRIES IN @lt_kontrh
        WHERE partner EQ @lt_kontrh-kontrh.

      DELETE gt_kontrh WHERE bu_group NE 'G008'.
      SORT: gt_kontrh BY kontrh.
    ENDIF.
  ENDIF.


ENDFORM. " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data .

  DATA ls_flow TYPE gty_flow.
  DATA ls_deal TYPE gty_deal.
  DATA ls_mess TYPE gty_mess.
  DATA lv_rfha_exclude TYPE tb_rfha.
  DATA lv_budat TYPE budat.

  CONCATENATE p_keydat(4) '01' '01' INTO lv_budat.
  lv_budat = lv_budat - 1.

  "Carry forward for beginning
  IF gs_class_cust-norev_yearend = 'X' AND gs_transfer-budat IS INITIAL.
    lv_budat = p_keydat.
  ENDIF.

  IF gs_class_cust-norev_yearend = 'X' AND gs_transfer-budat > lv_budat.
    lv_budat = gs_transfer-budat.
  ENDIF.

  SORT gt_flow BY bukrs rfha sbktyp.

  LOOP AT gt_flow INTO ls_flow.
    IF ls_flow-sbktyp = '11'.
      ls_flow-sbktyp = '13'.
      MODIFY gt_flow FROM ls_flow.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_flow INTO ls_flow.
    IF ls_flow-sbktyp = '12'.
      READ TABLE gt_flow WITH KEY bukrs = ls_flow-bukrs
                                  rfha  = ls_flow-rfha
                                  sbktyp = '13' TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_flow-sbktyp = '13'.
        MODIFY gt_flow FROM ls_flow.
      ENDIF.

      READ TABLE gt_flow WITH KEY bukrs = ls_flow-bukrs
                                  rfha  = ls_flow-rfha
                                  sbktyp = '14' TRANSPORTING NO FIELDS BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_flow-sbktyp = '14'.
        MODIFY gt_flow FROM ls_flow.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT gt_flow BY bukrs rfha.

  LOOP AT gt_flow INTO ls_flow.
    IF  ls_flow-sbktyp <> '10'.
      CONTINUE.
    ENDIF.
    CLEAR ls_deal.

    "If first investment has not been posted yet then local amount is initial then do not include that product
    IF ls_flow-khwkurs IS INITIAL AND ls_flow-dzterm <= p_keydat OR lv_rfha_exclude = ls_flow-rfha.
      IF lv_rfha_exclude = ls_flow-rfha.
        CONTINUE.
      ENDIF.
      ls_mess-msgty = 'W'.
      ls_mess-rfha = ls_flow-rfha.
      ls_mess-bukrs = ls_flow-bukrs.
      ls_mess-message = 'Yatırım gününde kayıt edilmemiş hareketler var, Ürün dahil edilmedi.'(025) ##TEXT_POOL.
      APPEND ls_mess TO gt_mess.

    ENDIF.

    ls_deal-bhwbetr = ls_flow-bhwbetr.
    ls_deal-bzbetr  =  ls_flow-bzbetr.
    ls_deal-bukrs  = ls_flow-bukrs.
    ls_deal-rfha = ls_flow-rfha.
    ls_deal-delfz = ls_flow-delfz.
    ls_deal-dblfz = ls_flow-dblfz.
    ls_deal-rfhazu = ls_flow-rfhazu.
    ls_deal-sgsart = ls_flow-sgsart.
    ls_deal-sfhaart = ls_flow-sfhaart.
    ls_deal-kontrh = ls_flow-kontrh.
    ls_deal-rportb = ls_flow-rportb.
    ls_deal-wzbetr = ls_flow-wzbetr.

    IF ls_flow-ssign = '-'.
      ls_deal-bhwbetr = ls_deal-bhwbetr * -1.
      ls_deal-bzbetr = ls_deal-bzbetr * -1.
    ENDIF.

    COLLECT  ls_deal INTO gt_deal.
  ENDLOOP.
  CLEAR : gs_ufrsson , ls_deal.
  LOOP AT gt_ufrsson INTO gs_ufrsson.
    READ TABLE gt_deal INTO ls_deal WITH KEY bukrs = gs_ufrsson-bukrs rfha = gs_ufrsson-rfha rfhazu = gs_ufrsson-rfhazu.
    IF sy-subrc EQ 0.


    ENDIF.
  ENDLOOP.


  LOOP AT gt_deal INTO ls_deal.
    IF ls_deal-delfz IS NOT INITIAL.
      SELECT SINGLE delfz INTO @ls_deal-delfz FROM vtbfhazu
      WHERE bukrs = @ls_deal-bukrs AND
      rfha  = @ls_deal-rfha AND
      rfhazu = @ls_deal-rfhazu.
    ENDIF.

    LOOP AT gt_flow INTO ls_flow.
      IF ls_flow-bukrs = ls_deal-bukrs AND ls_flow-rfha = ls_deal-rfha AND ( ls_flow-sbktyp = '12' OR ls_flow-sbktyp = '13' OR ls_flow-sbktyp = '14' ).
        ls_deal-sbktyp = ls_flow-sbktyp.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_budat > ls_deal-dblfz AND gs_class_cust-norev_yearend = 'X'.
      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
        EXPORTING
          date             = lv_budat
          foreign_amount   = ls_deal-bzbetr
          foreign_currency = ls_deal-wzbetr
          local_currency   = gs_t001-waers
        IMPORTING
          local_amount     = ls_deal-bhwbetr.
      .
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.

    MODIFY gt_deal FROM ls_deal.
  ENDLOOP.

ENDFORM. " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  INITIAL_OPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initial_ops .
  DATA ls_curtp TYPE gty_curtp.
  DATA ls_t005 TYPE t005.
  DATA ls_t000 TYPE t000.
  DATA ls_t880 TYPE t880.


  gv_budat_nextyear = p_keydat - 1.
  gv_budat_nextyear(4) =  gv_budat_nextyear(4) + 1.


*  SELECT SINGLE * FROM t001a INTO @gs_t001a
  SELECT SINGLE * FROM finsv_t001a INTO @gs_t001a
  WHERE bukrs IN @s_bukrs.

  SELECT SINGLE * FROM t001 INTO @gs_t001
  WHERE bukrs IN @s_bukrs.

  SELECT SINGLE * FROM t005 INTO @ls_t005
  WHERE land1 = @gs_t001-land1.

  SELECT SINGLE * FROM t000 INTO @ls_t000
  WHERE mandt = @sy-mandt.

  SELECT SINGLE * FROM t880 INTO @ls_t880
  WHERE rcomp = @gs_t001-rcomp.

  ls_curtp-curtp = '10'.
  ls_curtp-waers = gs_t001-waers.
  APPEND ls_curtp TO gt_curtp.

  ls_curtp-curtp = '30'.
  ls_curtp-waers = ls_t000-mwaer.
  APPEND ls_curtp TO gt_curtp.

  ls_curtp-curtp = '31'.
  ls_curtp-waers = ls_t000-mwaer.
  APPEND ls_curtp TO gt_curtp.

  ls_curtp-curtp = '32'.
  ls_curtp-waers = ls_t000-mwaer.
  APPEND ls_curtp TO gt_curtp.

  ls_curtp-curtp = '40'.
  ls_curtp-waers = ls_t005-curha.
  APPEND ls_curtp TO gt_curtp.

  ls_curtp-curtp = '50'.
  ls_curtp-waers = ls_t005-curin.
  APPEND ls_curtp TO gt_curtp.

  ls_curtp-curtp = '60'.
  ls_curtp-waers = ls_t880-curr.
  APPEND ls_curtp TO gt_curtp.


ENDFORM. " INITIAL_OPS
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_SHORT_TERM_FLOWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_short_term_flows .
  DATA ls_deal TYPE gty_deal.
  DATA ls_flow TYPE gty_flow.
  DATA ls_out TYPE gty_out.
  DATA ls_posted TYPE ztrm_ct0003.
  DATA lv_bzbetr_tot TYPE tb_bzbetr.
  DATA lv_bhwbetr_tot TYPE tb_bzbetr.
  DATA ls_class_acc TYPE ztrm_ct0004.
  DATA lv_exist TYPE c.

  DATA: lv_hkont_long_tr  TYPE char1,
        lv_hkont_short_tr TYPE char1.

  TYPES: BEGIN OF lty_add_upd,
           bukrs   TYPE bukrs,
           rfha    TYPE tb_rfha,
           zzclass TYPE ztrm_de_class,
         END OF lty_add_upd.

  DATA: lt_add_upd TYPE TABLE OF lty_add_upd,
        ls_add_upd TYPE lty_add_upd.

  "Short Term Installment Flows Calculation
  LOOP AT gt_deal INTO ls_deal.
    LOOP AT gt_flow INTO ls_flow.
      IF ls_flow-bukrs = ls_deal-bukrs AND ls_flow-rfha  = ls_deal-rfha AND
         ( ls_flow-sbktyp = '11' OR ls_flow-sbktyp = '12' OR ls_flow-sbktyp = '13' OR ls_flow-sbktyp = '14' OR ls_flow-sbktyp = '20' OR ls_flow-sbktyp = '24' ).

        IF ls_flow-sbktyp <> '20' AND ls_flow-sbktyp <> '24'.
          IF ls_deal-delfz GE gv_budat_nextyear.
            ls_out-zzclass = 'L'.
            IF    ls_flow-dzterm > p_keydat
              AND ls_flow-dzterm LE gv_budat_nextyear.
              ls_deal-bzbetr_short_current = ls_deal-bzbetr_short_current + ls_flow-bzbetr.
            ELSEIF ls_flow-dzterm > p_keydat
              AND ls_flow-dzterm GT gv_budat_nextyear.
              ls_deal-bzbetr_long = ls_deal-bzbetr_long + ls_flow-bzbetr.
            ENDIF.
          ELSE.
            ls_out-zzclass = 'S'.
            ls_deal-bzbetr_short = ls_deal-bzbetr_short + ls_flow-bzbetr.
            IF ls_flow-dzterm > p_keydat.
              ls_deal-bzbetr_short_current = ls_deal-bzbetr_short_current + ls_flow-bzbetr.
            ENDIF.
          ENDIF.
        ELSE.
          CONTINUE.
        ENDIF.

        IF ls_flow-dzterm > gv_budat_nextyear.
          CONTINUE.
        ENDIF.

        "This is valid for only leasing, because in leasing we keep capital and interest in same account
        IF ls_flow-sbktyp = '24'.
          ls_flow-sbktyp = '13'.
        ENDIF.

        CLEAR lv_exist.                         .
        LOOP AT gt_class_acc INTO ls_class_acc.
          IF ( ls_deal-bukrs CP ls_class_acc-bukrs OR ls_class_acc-bukrs = '' ) AND
          (  p_val CP ls_class_acc-val_area OR ls_class_acc-val_area = '' OR ls_deal-val_area CP ls_class_acc-val_area ) AND  "ls_deal-val_area
          ( ls_deal-sgsart CP ls_class_acc-sgsart OR ls_class_acc-sgsart = '' ) AND
          ( ls_deal-sfhaart CP ls_class_acc-sfhaart OR ls_class_acc-sfhaart = '' ) AND
          ( ls_flow-dis_flowtype CP ls_class_acc-dis_flowtype OR ls_class_acc-dis_flowtype = '' ) AND
          ( ls_flow-sbktyp CP ls_class_acc-sbktyp OR ls_class_acc-sbktyp = '' ) AND
          ( ls_flow-wzbetr CP ls_class_acc-wzbetr OR ls_class_acc-wzbetr = '' ).
            lv_exist = 'X'.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF lv_exist = ''.
          CONTINUE.
        ENDIF.
        APPEND ls_flow TO gt_detail.

        ls_out-keydat = p_keydat.
*        ls_out-val_area = p_val."" ertuge 17.09.2020 09:31:16 - hazard val area
        ls_out-val_area = ls_class_acc-val_area."" ertuge 17.09.2020 09:31:16 - hazard val area
        ls_out-dblfz = ls_deal-dblfz.
        ls_out-delfz = ls_deal-delfz.
        ls_out-bukrs = ls_deal-bukrs.
        ls_out-rfha = ls_deal-rfha.
        ls_out-sgsart = ls_deal-sgsart.
        ls_out-sfhaart = ls_deal-sfhaart.
        ls_out-kontrh = ls_deal-kontrh.
        ls_out-rportb = ls_deal-rportb.
        ls_out-sbktyp = ls_flow-sbktyp.
        ls_out-wzbetr = ls_flow-wzbetr.
        ls_out-bzbetr_short = ls_flow-bzbetr.
        ls_out-bhwbetr_short = ls_flow-bzbetr * ( ls_deal-bhwbetr / ls_deal-bzbetr ).
        ls_out-hkont_long = ls_class_acc-hkont_long.
        ls_out-hkont_short = ls_class_acc-hkont_short.
        ls_out-umskz_long = ls_class_acc-umskz_long.
        ls_out-umskz_short = ls_class_acc-umskz_short.


        IF ls_flow-sbktyp = '20'.
          ls_out-sbktyp_txt = 'Faiz'(023) ##TEXT_POOL.
          ls_out-bzbetr_short = ls_out-bzbetr_short * -1.
          ls_out-bhwbetr_short = ls_out-bhwbetr_short * -1 .
        ELSE.
          ls_out-sbktyp_txt = 'Anapara'(024) ##TEXT_POOL.
        ENDIF.
        COLLECT ls_out INTO gt_out.
      ENDIF.
    ENDLOOP.

    MODIFY gt_deal FROM ls_deal.
  ENDLOOP.
  CLEAR ls_out.
  "modify gt_out sbktyp 31 xmehmeto
  CLEAR gs_ufrsson.
  LOOP AT gt_ufrsson INTO gs_ufrsson.
    READ TABLE gt_out INTO ls_out WITH KEY bukrs = gs_ufrsson-bukrs rfha = gs_ufrsson-rfha.
    IF sy-subrc EQ 0.
      ls_out-dmbtr_val = gs_ufrsson-total_amortised.
      ls_out-sbktyp = '31'.

      CLEAR gs_ct0004.
      READ TABLE gt_ct0004 INTO gs_ct0004 WITH KEY bukrs = gs_ufrsson-bukrs sbktyp = gs_ufrsson-sbktyp sfhaart = gs_ufrsson-sfhaart sgsart = gs_ufrsson-sgsart.
      IF sy-subrc EQ 0.
        ls_out-hkont_short = gs_ct0004-hkont_short.
        ls_out-hkont_long = gs_ct0004-hkont_long.
        ls_out-umskz_short = gs_ct0004-umskz_short.
        ls_out-umskz_long = gs_ct0004-umskz_long.
      ELSE.
        ls_out-hkont_short = ''.
        ls_out-hkont_long = ''.
        ls_out-umskz_short = ''.
        ls_out-umskz_long = ''.

      ENDIF.
      APPEND ls_out TO gt_out.

    ENDIF.

  ENDLOOP.




  "Modify flows for capital  and postable  and posted amounts.
  SORT gt_db_posted BY bukrs rfha sbktyp.
  SORT gt_deal BY bukrs rfha.

  LOOP AT gt_out INTO ls_out.
    READ TABLE gt_deal INTO ls_deal WITH KEY bukrs = ls_out-bukrs
                                             rfha  = ls_out-rfha BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      ls_out-bzbetr_cap = ls_deal-bzbetr.
      ls_out-bhwbetr_cap = ls_deal-bhwbetr.
      MODIFY gt_out FROM ls_out.
    ENDIF.

    CLEAR lv_bhwbetr_tot.
    CLEAR lv_bzbetr_tot.

    LOOP AT gt_db_posted INTO ls_posted.
      IF ls_posted-bukrs = ls_out-bukrs AND ls_posted-rfha  = ls_out-rfha AND ls_posted-sbktyp = ls_out-sbktyp AND ls_posted-val_area = ls_out-val_area. "" ertuge 17.09.2020 10:12:49 - hazard val
        lv_bzbetr_tot = lv_bzbetr_tot + ls_posted-wrbtr.
        lv_bhwbetr_tot = lv_bhwbetr_tot + ls_posted-dmbtr.
      ENDIF.
    ENDLOOP.
    "xmehmeto start
    IF ls_out-sbktyp = '31'.
      CLEAR gs_ufrsson.
      READ TABLE gt_ufrsson INTO gs_ufrsson WITH KEY bukrs = ls_out-bukrs rfha = ls_out-rfha sbktyp = ls_out-sbktyp.
      IF sy-subrc EQ 0.
        ls_out-bzbetr_postable = gs_ufrsson-t_total.
        ls_out-bhwbetr_postable = gs_ufrsson-t_totalup.
        ls_out-bzbetr_short = gs_ufrsson-t_total.
        ls_out-bhwbetr_short = gs_ufrsson-t_totalup.

      ENDIF.
      "end
    ELSE.
      ls_out-bzbetr_posted = lv_bzbetr_tot.
      ls_out-bhwbetr_posted = lv_bhwbetr_tot.
      ls_out-bzbetr_postable = ls_out-bzbetr_short - ls_out-bzbetr_posted.
      ls_out-bhwbetr_postable = ls_out-bhwbetr_short - ls_out-bhwbetr_posted.

    ENDIF.



    SELECT SINGLE name_org1 FROM but000 INTO @ls_out-kontrh_txt
        WHERE partner = @ls_out-kontrh.

    CLEAR: lv_hkont_long_tr,
           lv_hkont_short_tr.

    READ TABLE gt_ct0013
      TRANSPORTING NO FIELDS
      WITH KEY hkont = ls_out-hkont_long
      BINARY SEARCH.
    IF sy-subrc EQ 0.
      lv_hkont_long_tr = 'X'.
    ENDIF.

    READ TABLE gt_ct0013
      TRANSPORTING NO FIELDS
      WITH KEY hkont = ls_out-hkont_short
      BINARY SEARCH.
    IF sy-subrc EQ 0.
      lv_hkont_short_tr = 'X'.
    ENDIF.

    IF   lv_hkont_long_tr  EQ 'X'
      OR lv_hkont_short_tr EQ 'X'.

      READ TABLE gt_kontrh
        ASSIGNING FIELD-SYMBOL(<lfs_kontrh>)
        WITH KEY kontrh = ls_out-kontrh
        BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF lv_hkont_long_tr EQ 'X'.
          ls_out-hkont_long = ls_out-hkont_long(6) && <lfs_kontrh>-bu_sort1(4).
        ENDIF.

        IF lv_hkont_short_tr EQ 'X'.
          ls_out-hkont_short = ls_out-hkont_short(6) && <lfs_kontrh>-bu_sort1(4).
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY gt_out FROM ls_out.
  ENDLOOP.




ENDFORM. " CALCULATE_SHORT_TERM_FLOWS


FORM fill_fcat USING us_lvc_cat TYPE lvc_s_fcat
                     uv_fname
                     uv_tabname
                     uv_ref_fname
                     uv_ref_tab
                     uv_col_text.

  DATA ls_lvc_cat TYPE lvc_s_fcat.

  ls_lvc_cat-fieldname = uv_fname.
  ls_lvc_cat-tabname   = uv_tabname.
  ls_lvc_cat-ref_table = uv_ref_tab.
  ls_lvc_cat-ref_field = uv_ref_fname.
  ls_lvc_cat-coltext   = uv_col_text.
  ls_lvc_cat-scrtext_l = uv_col_text.
  ls_lvc_cat-scrtext_m = uv_col_text.
  ls_lvc_cat-scrtext_s = uv_col_text.


  us_lvc_cat = ls_lvc_cat.



ENDFORM. " DYNAMIC_TABLE_FCAT


FORM prepare_fcat_summary USING ut_fcat TYPE lvc_t_fcat.
  DATA ls_fcat TYPE lvc_s_fcat.

  PERFORM fill_fcat USING ls_fcat 'CHECK' 'GT_OUT' '' '' 'Check'(022) ##TEXT_POOL.
  ls_fcat-checkbox = 'X'.
  ls_fcat-edit = 'X'.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'BUKRS' 'GT_OUT' 'BUKRS' 'BKPF' '' ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'VAL_AREA' 'GT_OUT' 'VALUATION_AREA' 'TRACT_DOCUMENT' '' ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'ACC_PRINCIPLE' 'GT_OUT' 'ACC_PRINCIPLE' 'TRACT_DOCUMENT' '' ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'RFHA' 'GT_OUT' 'RFHA' 'VTBFHAPO' '' ##TEXT_POOL.
  ls_fcat-hotspot = 'X'.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'KONTRH' 'GT_OUT' 'KONTRH' 'VTBFHA' '' ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'RPORTB' 'GT_OUT' 'RPORTB' 'VTBFHA' '' ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'KONTRH_TXT' 'GT_OUT' 'NAME_ORG1' 'BUT000' '' ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'SGSART' 'GT_OUT' 'SGSART' 'VTBFHA' '' ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'SFHAART' 'GT_OUT' 'SFHAART' 'VTBFHA' '' ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'KEYDAT' 'GT_OUT' 'BUDAT' 'BKPF' 'Anahtar Tarih'(021) ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'SBKTYP' 'GT_OUT' 'VTBFHAPO' 'SBKTYP' 'Kayıt Tipi'(001) ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'SBKTYP_TXT' 'GT_OUT' '' '' 'Kayıt Tipi Tanım'(020) ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'BZBETR_CAP' 'GT_OUT' 'BZBETR' 'VTBFHAPO' 'Kredi Tutarı(BPB)'(019) ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'WZBETR' 'GT_OUT' 'WZBETR' 'VTBFHAPO' 'PB' ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'BHWBETR_CAP' 'GT_OUT' 'BZBETR' 'VTBFHAPO' 'Kredi Tutarı(UP)'(018) ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'DMBTR_VAL' 'GT_OUT' 'DMBTR' 'BSEG' 'Aylık Toplam Değ'(017) ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'HWAER_VAL' 'GT_OUT' 'WAERS' 'BSEG' 'Değerlenen(PB)'(016) ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'BZBETR_SHORT' 'GT_OUT' 'BZBETR' 'VTBFHAPO' 'Kısa Vade(BPB)'(015) ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'BHWBETR_SHORT' 'GT_OUT' 'BZBETR' 'VTBFHAPO' 'Kısa Vade(UP)'(014) ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'BZBETR_POSTED' 'GT_OUT' 'BZBETR' 'VTBFHAPO' 'Kısa Vade Kaydedilmiş(BPB)'(013) ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'BHWBETR_POSTED' 'GT_OUT' 'BZBETR' 'VTBFHAPO' 'Kısa Vade Kaydedilmiş(UP)'(012) ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'BZBETR_POSTABLE' 'GT_OUT' 'BZBETR' 'VTBFHAPO' 'Kayıt Edilecek(BPB)'(011) ##TEXT_POOL.
  ls_fcat-hotspot = 'X'.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'BHWBETR_POSTABLE' 'GT_OUT' 'BZBETR' 'VTBFHAPO' 'Kayıt Edilecek(UP)'(010) ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'HKONT_SHORT' 'GT_OUT' 'HKONT_SHORT' 'ZTRM_CT0004' '' ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'HKONT_LONG' 'GT_OUT' 'HKONT_LONG' 'ZTRM_CT0004' '' ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'BELNR' 'GT_OUT' 'BELNR' 'BKPF' '' ##TEXT_POOL.
  ls_fcat-hotspot = 'X'.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'BUDAT' 'GT_OUT' 'BUDAT' 'BKPF' '' ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

  PERFORM fill_fcat USING ls_fcat 'ZZCLASS' 'GT_OUT' 'ZZCLASS' 'VTBFHA' '' ##TEXT_POOL.
  APPEND ls_fcat TO ut_fcat.

ENDFORM. " PREPARE_FCAT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_output USING ut_fcat TYPE lvc_t_fcat.

  DATA ls_layout TYPE lvc_s_layo.
  DATA ls_variant TYPE disvariant.
  DATA ls_fcat TYPE lvc_s_fcat.
  DATA lv_pf_status TYPE slis_formname.
  FIELD-SYMBOLS <lfs_table> TYPE STANDARD TABLE.


  READ TABLE ut_fcat INTO ls_fcat INDEX 1.
  IF sy-subrc IS INITIAL.
    IF ls_fcat-tabname = 'GT_OUT'.
      lv_pf_status = 'SET_PF_STATUS'.
    ENDIF.
    ASSIGN (ls_fcat-tabname) TO <lfs_table>.
  ELSE.
    RETURN.
  ENDIF.

  ls_layout-cwidth_opt = 'X'.

  ls_variant-report = sy-repid.
  ls_variant-handle = ls_fcat-tabname+3.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = lv_pf_status
      i_callback_user_command  = 'USER_COMMAND'
    " I_STRUCTURE_NAME         = 'GT_OUTPUT_SUM'
      is_layout_lvc            = ls_layout
      it_fieldcat_lvc          = ut_fcat
      "IT_SORT_LVC                       =
      "I_DEFAULT                         = 'X'
      i_save                   = 'A'
      is_variant               = ls_variant
    TABLES
      t_outtab                 = <lfs_table>.

ENDFORM. " DISPLAY_OUPUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_message .
  DATA lt_fcat TYPE lvc_t_fcat.
  DATA ls_fcat TYPE lvc_s_fcat.
  DATA ls_layout TYPE lvc_s_layo.
  DATA ls_variant TYPE disvariant.

  ls_layout-cwidth_opt = 'X'.

  ls_variant-report = sy-repid.
  ls_variant-handle = 'Mess'(009) ##TEXT_POOL.

  PERFORM fill_fcat USING ls_fcat 'MSGTY' 'GT_MESS' 'MSGTY' 'SYST' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'MSGID' 'GT_MESS' 'MSGID' 'SYST' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'MSGNO' 'GT_MESS' 'MSGNO' 'SYST' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'BUKRS' 'GT_MESS' 'BUKRS' 'BKPF' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'BELNR' 'GT_MESS' 'BELNR' 'BKPF' '' ##TEXT_POOL.
  ls_fcat-hotspot = 'X'.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'GJAHR' 'GT_MESS' 'GJAHR' 'BKPF' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'RFHA'  'GT_MESS' 'RFHA' 'VTBFHA' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'MESSAGE' 'GT_MESS' '' '' 'Message'(008) ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program      = sy-repid
      i_callback_user_command = 'USER_COMMAND'
      i_screen_start_column   = 5
      i_screen_start_line     = 1
      i_screen_end_column     = 120
      i_screen_end_line       = 20
      it_fieldcat_lvc         = lt_fcat
      is_layout_lvc           = ls_layout
      is_variant              = ls_variant
      i_save                  = 'A'
    TABLES
      t_outtab                = gt_mess.     " I_CALLBACK_USER_COMMAND = 'USER_COMMAND'


ENDFORM. " DISPLAY_MESSAGE


FORM set_pf_status USING ut_extab TYPE slis_t_extab.        "#EC CALLED
  DATA lv_budat_end TYPE budat.
  DATA lv_nopost TYPE c.
  DATA lv_notransfer TYPE c.
  DATA ls_mess TYPE gty_mess.

  LOOP AT gt_mess INTO ls_mess.
    IF ls_mess-msgty = 'E'.
      lv_nopost = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = p_keydat
    IMPORTING
      e_date = lv_budat_end.

  IF p_keydat <> lv_budat_end.
    lv_nopost = 'X'.
  ENDIF.

  IF p_keydat > gs_transfer-budat AND gs_transfer-budat IS NOT INITIAL.
    lv_notransfer = 'X'.
  ELSE.
    lv_nopost = 'X'.
  ENDIF.

  IF lv_nopost = 'X'.
*    APPEND '&POST' TO ut_extab.
    APPEND 'POST' TO ut_extab.
  ENDIF.


  IF lv_notransfer = 'X'.
    APPEND '&CFORWARD' TO ut_extab..
  ENDIF.

  SET PF-STATUS 'STANDARD' EXCLUDING ut_extab ##STAT_UNDEF.

ENDFORM. "set_pf_status


FORM user_command USING uv_ucomm LIKE sy-ucomm
                        us_selfield TYPE slis_selfield.

  DATA: ls_mess TYPE gty_mess.
  DATA: ls_out TYPE gty_out.
  FIELD-SYMBOLS <lfs_table> TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lfs_line>.
  FIELD-SYMBOLS <lfs_bukrs> TYPE bukrs.
  FIELD-SYMBOLS <lfs_belnr> TYPE belnr_d.
  FIELD-SYMBOLS <lfs_gjahr> TYPE gjahr.
  FIELD-SYMBOLS <lfs_rfha>  TYPE tb_rfha.
  FIELD-SYMBOLS <lfs_budat> TYPE budat.
  DATA lt_filtered  TYPE lvc_t_fidx.
  DATA lv_error TYPE c.


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = go_ref.
  CALL METHOD go_ref->check_changed_data.

  us_selfield-refresh = 'X'.

  ASSIGN (us_selfield-tabname) TO <lfs_table>.
  IF <lfs_table> IS ASSIGNED.
    READ TABLE <lfs_table> ASSIGNING <lfs_line> INDEX us_selfield-tabindex.
    IF sy-subrc IS INITIAL.
      ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <lfs_line> TO <lfs_bukrs>.
      ASSIGN COMPONENT 'BELNR' OF STRUCTURE <lfs_line> TO <lfs_belnr>.
      ASSIGN COMPONENT 'GJAHR' OF STRUCTURE <lfs_line> TO <lfs_gjahr>.
      ASSIGN COMPONENT 'RFHA' OF STRUCTURE <lfs_line> TO <lfs_rfha>.
      ASSIGN COMPONENT 'BUDAT' OF STRUCTURE <lfs_line> TO <lfs_budat>.
    ENDIF.
  ENDIF.


  CASE uv_ucomm.
    WHEN '&IC1'.
      IF ( us_selfield-tabname = 'GT_MESS' OR us_selfield-tabname = 'GT_CLASS_LOG' ) AND us_selfield-fieldname = 'BELNR'.
        SET PARAMETER ID 'BUK' FIELD <lfs_bukrs>.
        SET PARAMETER ID 'GJR' FIELD <lfs_gjahr>.
        SET PARAMETER ID 'BLN' FIELD <lfs_belnr>.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.   "#EC CI_CALLTA
      ELSEIF us_selfield-tabname = 'GT_OUT' AND us_selfield-fieldname = 'BELNR'.
        SET PARAMETER ID 'BUK' FIELD  <lfs_bukrs>.
        SET PARAMETER ID 'GJR' FIELD <lfs_budat>(4).
        SET PARAMETER ID 'BLN' FIELD <lfs_belnr>.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.   "#EC CI_CALLTA
      ELSEIF us_selfield-tabname = 'GT_OUT' AND us_selfield-fieldname = 'BZBETR_POSTABLE'.
        ls_out = <lfs_line>.                 "#EC CI_FLDEXT_OK[2610650]
        PERFORM display_detail USING ls_out.
      ELSEIF us_selfield-fieldname = 'RFHA'.
        SET PARAMETER ID 'BUK' FIELD <lfs_bukrs>.
        SET PARAMETER ID 'FAN' FIELD <lfs_rfha>.
        CALL TRANSACTION 'FTR_DISPLAY' AND SKIP FIRST SCREEN . "#EC CI_CALLTA
      ENDIF.
    WHEN 'POST'.
      IF us_selfield-tabname = 'GT_OUT'.
        PERFORM post_to_accounting USING 'X'.
        LOOP AT gt_mess INTO ls_mess.
          IF ls_mess-msgty = 'E'.
            lv_error = 'X'.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF lv_error IS INITIAL.
          CLEAR gt_mess.
          PERFORM post_to_accounting USING ''.
        ENDIF.

        PERFORM display_message.
      ENDIF.
    WHEN '&CFORWARD'.
      PERFORM make_carryforward.

    WHEN '&SELALL'.
      CALL METHOD go_ref->get_filtered_entries
        IMPORTING
          et_filtered_entries = lt_filtered.

      SORT lt_filtered BY table_line.
      LOOP AT gt_out INTO ls_out.
        READ TABLE lt_filtered WITH KEY table_line = sy-tabix TRANSPORTING NO FIELDS.
        IF sy-subrc IS NOT INITIAL.
          ls_out-check = 'X'.
          MODIFY gt_out FROM ls_out.
        ENDIF.
      ENDLOOP.

    WHEN '&DESELALL'.
      CALL METHOD go_ref->get_filtered_entries
        IMPORTING
          et_filtered_entries = lt_filtered.

      LOOP AT gt_out INTO ls_out.
        READ TABLE lt_filtered WITH KEY table_line = sy-tabix TRANSPORTING NO FIELDS.
        IF sy-subrc IS NOT INITIAL.
          ls_out-check = ' '.
          MODIFY gt_out FROM ls_out.
        ENDIF.
      ENDLOOP.


  ENDCASE.

ENDFORM. "user_command


*&---------------------------------------------------------------------*
*&      Form  POST_TO_ACCOUNTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_to_accounting USING uv_test.

  DATA ls_out TYPE gty_out.
  DATA ls_docheader TYPE bapiache09.
  DATA lt_accgl TYPE TABLE OF bapiacgl09.
  DATA ls_accgl TYPE bapiacgl09.
  DATA: lt_accpay TYPE TABLE OF bapiacap09,
        ls_accpay TYPE bapiacap09,
        lt_accres TYPE TABLE OF bapiacar09,
        ls_accres TYPE  bapiacar09.

  DATA lt_ext1 TYPE TABLE OF bapiacextc.
  DATA ls_ext1 TYPE bapiacextc.
  DATA: lt_curramt TYPE TABLE OF bapiaccr09.
  DATA: ls_curramt TYPE bapiaccr09.
  DATA: lt_return TYPE TABLE OF bapiret2.
  DATA: ls_return TYPE bapiret2.
  DATA: lv_buzei TYPE bseg-buzei.
  DATA: ls_mess TYPE gty_mess.
  DATA: lv_objkey TYPE bapiache09-obj_key.
  DATA: lv_err TYPE c.
  DATA: ls_tac_va TYPE trgc_tac_va.
  DATA: ls_curtp TYPE gty_curtp.
  DATA: lt_curtp_cur TYPE gtt_curtp.
  DATA: ls_curtp_cur TYPE gty_curtp.
  DATA: lv_deal_number LIKE ls_out-rfha .
  DATA: lv_flag_pexist.
  DATA: lt_tac_va TYPE TABLE OF trgc_tac_va.
  DATA: lv_error TYPE c.
  DATA: lv_segment TYPE bapiacgl09-segment.

  SELECT mandt,
         tr_acc_code,
         valuation_area,
         valuation_curr,
         ledger_selection,
         special_ledger,
         rate_cat,
         ledger_category,
         acc_principle,
         src_var_sec,
         curr_type,
         transl_cat FROM trgc_tac_va INTO TABLE @lt_tac_va.

  SORT lt_tac_va BY tr_acc_code valuation_area.

  ls_curtp-curtp = '10'.
  APPEND ls_curtp TO lt_curtp_cur.

  IF gs_t001a-curtp IS NOT INITIAL.
    ls_curtp-curtp = gs_t001a-curtp.
    APPEND ls_curtp TO lt_curtp_cur.
  ENDIF.

  IF gs_t001a-curtp2 IS NOT INITIAL.
    ls_curtp-curtp = gs_t001a-curtp2.
    APPEND ls_curtp TO lt_curtp_cur.
  ENDIF.

  SORT gt_curtp BY curtp.

  LOOP AT gt_out INTO ls_out.
    IF NOT ( ls_out-check = 'X' AND ls_out-bhwbetr_postable <> 0 AND ls_out-belnr IS INITIAL ).
      CONTINUE.
    ENDIF.

    lv_flag_pexist = 'X'.

    CLEAR: ls_docheader, lt_accgl, ls_accgl, ls_accpay, lt_accpay, lt_curramt, ls_curramt, lt_return, ls_return, lt_ext1, ls_ext1,
        lt_accres,ls_accres.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_out-rfha
      IMPORTING
        output = lv_deal_number.

    ls_docheader-bus_act = 'RFBU'.
    ls_docheader-comp_code = ls_out-bukrs.
    ls_docheader-pstng_date = p_post.
    ls_docheader-doc_date = p_post.
    IF p_post LE '20201231'.
      ls_docheader-doc_type = 'OB'.
    ELSE.
      ls_docheader-doc_type = gc_blart.
    ENDIF.
*      ls_docheader-doc_type = gc_blart.
    ls_docheader-username = sy-uname.
    ls_docheader-header_txt = lv_deal_number.
    ls_docheader-ref_doc_no = 'UZUN-KISA TRANSFER' ##TEXT_POOL.
    lv_buzei = '001'.

    IF gs_db_val-xfeld = 'X' AND ls_out-sbktyp = '30'.
      ls_docheader-acc_principle = ls_out-acc_principle.
    ENDIF.


    READ TABLE lt_tac_va INTO ls_tac_va WITH KEY tr_acc_code = ls_out-bukrs valuation_area = ls_out-val_area BINARY SEARCH. " valuation_area = p_val
    IF sy-subrc IS INITIAL.
      ls_docheader-acc_principle = ls_tac_va-acc_principle.
    ENDIF.

    IF ls_out-umskz_long IS NOT INITIAL.
      CLEAR ls_accpay.
      ls_accpay-itemno_acc = lv_buzei.
      ls_accpay-vendor_no  = ls_out-kontrh.
      ls_accpay-sp_gl_ind  = ls_out-umskz_long.
      CONCATENATE 'KREDİ UZUN KISA DEĞERLEME GEÇİŞİ: ' lv_deal_number INTO ls_accpay-item_text ##TEXT_POOL.
      ls_accpay-alloc_nmbr = lv_deal_number.
      ls_accpay-profit_ctr = ls_out-rportb.
*        SELECT SINGLE target1 FROM k9rs4d1000007 INTO lv_segment
*          WHERE sour1_from EQ ls_out-rportb AND sour1_to EQ ls_out-rportb.
*        ls_accpay-segment = lv_segment.
      APPEND ls_accpay TO lt_accpay.
    ELSE.
      ls_accgl-itemno_acc = lv_buzei.
      ls_accgl-gl_account = ls_out-hkont_long.
      IF p_post LE '20201231'.
        ls_accgl-ref_key_2 = 'OB'.
      ELSE.
        ls_accgl-ref_key_2 = gc_blart.
      ENDIF.
*        ls_accgl-ref_key_2 = gc_blart.
      CONCATENATE 'KREDİ UZUN KISA DEĞERLEME GEÇİŞİ: ' lv_deal_number INTO ls_accgl-item_text ##TEXT_POOL.
      ls_accgl-alloc_nmbr = lv_deal_number.
      ls_accgl-profit_ctr = ls_out-rportb.
*        SELECT SINGLE target1 FROM k9rs4d1000007 INTO lv_segment
*          WHERE sour1_from EQ ls_out-rportb AND sour1_to EQ ls_out-rportb.
*        ls_accgl-segment = lv_segment.

      APPEND ls_accgl TO lt_accgl.
    ENDIF.


    IF gs_db_val-xfeld = 'X' AND ls_out-sbktyp = '30'.
      READ TABLE lt_tac_va WITH KEY  tr_acc_code = ls_out-bukrs
                                     valuation_area = ls_out-val_area BINARY SEARCH INTO ls_tac_va.

      ls_curramt-itemno_acc = lv_buzei.
      ls_curramt-currency = ls_out-wzbetr.
      ls_curramt-curr_type = '00'.
      ls_curramt-amt_doccur = ls_out-bzbetr_postable.
      APPEND ls_curramt TO lt_curramt.

      READ TABLE gt_curtp WITH KEY curtp = ls_tac_va-curr_type INTO ls_curtp BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_curramt-itemno_acc = lv_buzei.
        ls_curramt-currency   = ls_curtp-waers.
        ls_curramt-curr_type  = ls_curtp-curtp.
        ls_curramt-amt_doccur = ls_out-bhwbetr_postable.
        APPEND ls_curramt TO lt_curramt.
      ENDIF.

      SORT lt_curramt BY itemno_acc curr_type.
      LOOP AT lt_curtp_cur INTO ls_curtp_cur.
        READ TABLE lt_curramt WITH KEY itemno_acc = lv_buzei
                                       curr_type  = ls_curtp_cur-curtp TRANSPORTING NO FIELDS BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          READ TABLE gt_curtp WITH KEY curtp = ls_curtp_cur-curtp INTO ls_curtp BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ls_curramt-itemno_acc = lv_buzei.
            ls_curramt-currency   = ls_curtp-waers.
            ls_curramt-curr_type  = ls_curtp-curtp.
            ls_curramt-amt_doccur = 0.
            APPEND ls_curramt TO lt_curramt.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      ls_curramt-itemno_acc = lv_buzei.
      ls_curramt-currency = ls_out-wzbetr.
      ls_curramt-curr_type = '00'.
      ls_curramt-amt_doccur = ls_out-bzbetr_postable.
      APPEND ls_curramt TO lt_curramt.

      ls_curramt-itemno_acc = lv_buzei.
      ls_curramt-currency = gs_t001-waers.
      ls_curramt-curr_type = '10'.
      ls_curramt-amt_doccur = ls_out-bhwbetr_postable.
      APPEND ls_curramt TO lt_curramt.
    ENDIF.


    lv_buzei = lv_buzei + 1.
    CLEAR: ls_accgl, ls_curramt, lv_segment .

    IF ls_out-umskz_short IS NOT INITIAL.
      CLEAR ls_accpay.
      ls_accpay-itemno_acc = lv_buzei.
      ls_accpay-vendor_no  = ls_out-kontrh.
      ls_accpay-sp_gl_ind  = ls_out-umskz_short.
      CONCATENATE 'KREDİ UZUN KISA DEĞERLEME GEÇİŞİ: ' lv_deal_number INTO ls_accpay-item_text ##TEXT_POOL.
      ls_accpay-alloc_nmbr = lv_deal_number.
      ls_accpay-profit_ctr = ls_out-rportb.
*        SELECT SINGLE target1 FROM k9rs4d1000007 INTO lv_segment
*          WHERE sour1_from EQ ls_out-rportb AND sour1_to EQ ls_out-rportb.
*        ls_accpay-segment = lv_segment.
      APPEND ls_accpay TO lt_accpay.
    ELSE.
      ls_accgl-itemno_acc = lv_buzei.
      ls_accgl-gl_account = ls_out-hkont_short.
      CONCATENATE 'KREDİ UZUN KISA DEGERLEME GEÇİŞİ: ' lv_deal_number INTO ls_accgl-item_text ##TEXT_POOL.
      IF p_post LE '20201231'.
        ls_accgl-ref_key_2 = 'OB'.
      ELSE.
        ls_accgl-ref_key_2 = gc_blart.
      ENDIF.
*        ls_accgl-ref_key_2 = gc_blart.
      ls_accgl-alloc_nmbr = lv_deal_number.
      ls_accgl-profit_ctr = ls_out-rportb.
*        SELECT SINGLE target1 FROM k9rs4d1000007 INTO lv_segment
*          WHERE sour1_from EQ ls_out-rportb AND sour1_to EQ ls_out-rportb.
*        ls_accgl-segment = lv_segment.
      APPEND ls_accgl TO lt_accgl.
    ENDIF.

    CLEAR: lv_segment .

    IF gs_db_val-xfeld = 'X' AND ls_out-sbktyp = '30'.
      READ TABLE lt_tac_va INTO ls_tac_va WITH KEY tr_acc_code = ls_out-bukrs
                                                   valuation_area = ls_out-val_area BINARY SEARCH.
      ls_curramt-itemno_acc = lv_buzei.
      ls_curramt-currency = ls_out-wzbetr.
      ls_curramt-curr_type = '00'.
      ls_curramt-amt_doccur = ls_out-bzbetr_postable * -1.
      APPEND ls_curramt TO lt_curramt.

      READ TABLE gt_curtp WITH KEY curtp = ls_tac_va-curr_type INTO ls_curtp BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        ls_curramt-itemno_acc = lv_buzei.
        ls_curramt-currency   = ls_curtp-waers.
        ls_curramt-curr_type  = ls_curtp-curtp.
        ls_curramt-amt_doccur = ls_out-bhwbetr_postable * -1.
        APPEND ls_curramt TO lt_curramt.
      ENDIF.

      SORT lt_curramt BY itemno_acc curr_type.

      LOOP AT lt_curtp_cur INTO ls_curtp_cur.
        READ TABLE lt_curramt WITH KEY itemno_acc = lv_buzei
                                       curr_type  = ls_curtp_cur-curtp TRANSPORTING NO FIELDS BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          READ TABLE gt_curtp WITH KEY curtp = ls_curtp_cur-curtp INTO ls_curtp BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            ls_curramt-itemno_acc = lv_buzei.
            ls_curramt-currency   = ls_curtp-waers.
            ls_curramt-curr_type  = ls_curtp-curtp.
            ls_curramt-amt_doccur = 0.
            APPEND ls_curramt TO lt_curramt.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      ls_curramt-itemno_acc = lv_buzei.
      ls_curramt-currency = ls_out-wzbetr.
      ls_curramt-curr_type = '00'.
      ls_curramt-amt_doccur = ls_out-bzbetr_postable * -1.
      APPEND ls_curramt TO lt_curramt.

      ls_curramt-itemno_acc = lv_buzei.
      ls_curramt-currency = gs_t001-waers.
      ls_curramt-curr_type = '10'.
      ls_curramt-amt_doccur = ls_out-bhwbetr_postable * -1.
      APPEND ls_curramt TO lt_curramt.
    ENDIF.

    CONCATENATE 'VERTN:' ls_out-rfha INTO ls_ext1.
    APPEND ls_ext1 TO lt_ext1.

    IF ls_out-sfhaart(1) = '1'.
      LOOP AT lt_accpay INTO DATA(ls_acc).
        MOVE-CORRESPONDING ls_acc TO ls_accres.
        ls_accres-customer = ls_acc-vendor_no.
        APPEND ls_accres TO lt_accres.
      ENDLOOP.
      CLEAR:lt_accpay,ls_accres.
    ENDIF.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader    = ls_docheader
      IMPORTING
        obj_key           = lv_objkey
      TABLES
        accountgl         = lt_accgl
        accountpayable    = lt_accpay
        accountreceivable = lt_accres
        currencyamount    = lt_curramt
        extension1        = lt_ext1
        return            = lt_return.

    CLEAR lv_error.
    LOOP AT lt_return INTO ls_return.
      IF ls_return-type = 'E'.
        lv_error = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF lv_error IS INITIAL AND lv_objkey <> '$'.
      LOOP AT lt_return INTO ls_return.
        IF ls_return-type = 'S'.
          CLEAR ls_mess.
          ls_mess-rfha = ls_out-rfha.
          ls_mess-bukrs = lv_objkey+10(4).
          ls_mess-belnr = lv_objkey.
          ls_mess-gjahr = lv_objkey+14(4).
          ls_mess-msgty = ls_return-type.
          ls_mess-message = ls_return-message.
          ls_mess-msgid = ls_return-id.
          ls_mess-msgno = ls_return-number.
          APPEND ls_mess TO gt_mess.
        ENDIF.
      ENDLOOP.
      IF uv_test IS INITIAL.
        ls_out-belnr = lv_objkey.
        ls_out-budat = p_keydat.
        PERFORM update_tables USING ls_out lv_err.
        IF lv_err = ''.
          MODIFY gt_out FROM ls_out.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.


          UPDATE vtbfha SET zzclass = ls_out-zzclass
           WHERE bukrs  EQ ls_out-bukrs
             AND rfha   EQ ls_out-rfha.
          IF sy-subrc IS INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ENDIF.


        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      LOOP AT lt_return INTO ls_return.
        IF ls_return-type = 'E'.
          CLEAR ls_mess.
          ls_mess-msgty = ls_return-type.
          ls_mess-msgno = ls_return-number.
          ls_mess-msgid = ls_return-id.
          ls_mess-message = ls_return-message.
          APPEND ls_mess TO gt_mess.
        ENDIF.
      ENDLOOP.
    ENDIF.

    """""""""""""""Make reversal of first posting because this is valuation posting"""""""""""""""""""""
    IF ls_out-sbktyp = '30'.
      CLEAR: lt_return, ls_return, ls_mess, lv_objkey.

      ls_docheader-trans_date = ls_docheader-pstng_date.
      ls_docheader-pstng_date = ls_docheader-pstng_date + 1.
      ls_docheader-doc_date = ls_docheader-doc_date + 1.

      LOOP AT lt_curramt INTO ls_curramt.
        ls_curramt-amt_doccur = ls_curramt-amt_doccur * -1.
        MODIFY lt_curramt FROM ls_curramt.
      ENDLOOP.

      CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
        EXPORTING
          documentheader = ls_docheader
        IMPORTING
          obj_key        = lv_objkey
        TABLES
          accountgl      = lt_accgl
          accountpayable = lt_accpay
          currencyamount = lt_curramt
          extension1     = lt_ext1
          return         = lt_return.

      CLEAR lv_error.
      LOOP AT lt_return INTO ls_return.
        IF ls_return-type = 'E'.
          lv_error = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_error IS INITIAL AND lv_objkey <> '$'.
        LOOP AT lt_return INTO ls_return.
          IF ls_return-type = 'S'.
            CLEAR ls_mess.
            ls_mess-rfha = ls_out-rfha.
            ls_mess-bukrs = lv_objkey+10(4).
            ls_mess-belnr = lv_objkey.
            ls_mess-gjahr = lv_objkey+14(4).
            ls_mess-msgty = ls_return-type.
            ls_mess-message = ls_return-message.
            ls_mess-msgid = ls_return-id.
            ls_mess-msgno = ls_return-number.
            APPEND ls_mess TO gt_mess.
          ENDIF.
        ENDLOOP.
        IF uv_test IS INITIAL.
          ls_out-belnr_rev = lv_objkey.
          ls_out-budat_rev = ls_docheader-pstng_date.
          PERFORM update_tables USING ls_out lv_err.
          IF lv_err = ''.
            MODIFY gt_out FROM ls_out.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ENDIF.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        LOOP AT lt_return INTO ls_return.
          IF ls_return-type = 'E'.
            CLEAR ls_mess.
            ls_mess-msgty = ls_return-type.
            ls_mess-msgno = ls_return-number.
            ls_mess-msgid = ls_return-id.
            ls_mess-message = ls_return-message.
            APPEND ls_mess TO gt_mess.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lv_flag_pexist IS INITIAL.
    CLEAR ls_mess.
    ls_mess-msgty = 'S'.
    ls_mess-message = 'Kaydedilecek tutar yok'(007) ##TEXT_POOL.
    APPEND ls_mess TO gt_mess.
  ENDIF.



ENDFORM. " POST_TO_ACCOUNTING
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OUT  text
*      -->P_LV_ERR  text
*----------------------------------------------------------------------*
FORM update_tables USING us_out TYPE gty_out
                         us_err TYPE char1.

  DATA ls_class_acc TYPE ztrm_ct0003.
  DATA ls_mess TYPE gty_mess.


  ls_class_acc-bukrs      = us_out-bukrs.
  ls_class_acc-belnr      = us_out-belnr.
  ls_class_acc-gjahr      = us_out-budat(4).
  ls_class_acc-keydat     = us_out-keydat.
  ls_class_acc-budat      = us_out-budat.
  ls_class_acc-sbktyp     = us_out-sbktyp.
  ls_class_acc-val_area   = us_out-val_area.
  ls_class_acc-hkont_long = us_out-hkont_long.
  ls_class_acc-rportb     = us_out-rportb.
  ls_class_acc-rfha       = us_out-rfha.
  ls_class_acc-waers      = us_out-wzbetr.
  ls_class_acc-wrbtr      = us_out-bzbetr_postable.
  ls_class_acc-dmbtr      = us_out-bhwbetr_postable.
  ls_class_acc-belnr_rev  = us_out-belnr_rev.
  ls_class_acc-budat_rev  = us_out-budat_rev.
  MODIFY ztrm_ct0003 FROM ls_class_acc.
  IF sy-subrc IS INITIAL.
    us_err = ''.
  ELSE.
    ls_mess-msgty = 'E'.
    ls_mess-bukrs = us_out-bukrs.
    ls_mess-belnr = us_out-belnr.
    ls_mess-gjahr = us_out-budat(4).
    ls_mess-rfha  = us_out-rfha.
    ls_mess-message = 'Log tablosu güncellenemediği için belge oluşturulamadı'(006) ##TEXT_POOL.
    APPEND ls_mess TO gt_mess.
    us_err = 'X'.
  ENDIF.

ENDFORM. " UPDATE_TABLES
*&---------------------------------------------------------------------*
*&      Form  GET_PERIODIC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_periodic_data .
  DATA lt_pos_ident TYPE TABLE OF dift_pos_ident.
  DATA lt_poscontext TYPE TABLE OF tract_poscontext.
  DATA lt_accitem TYPE TABLE OF tract_accitem.
  DATA lt_document TYPE TABLE OF tract_document.
  DATA ls_document TYPE tract_document.
  DATA ls_accitem TYPE tract_accitem.
  DATA ls_deal TYPE gty_deal.
  DATA ls_out TYPE gty_out.
  DATA lv_bzbetr_tot TYPE tb_bzbetr.
  DATA lv_bhwbetr_tot TYPE tb_bzbetr.
  DATA ls_posted TYPE ztrm_ct0003.
  DATA ls_detail TYPE gty_flow.
  DATA ls_class_acc TYPE ztrm_ct0004.
  DATA lv_exist TYPE c.

  DATA: lr_docgu LIKE RANGE OF tract_accitem-document_guid,
        lr_itmnu LIKE RANGE OF tract_accitem-item_number.

  IF gt_deal IS NOT INITIAL.
    IF gs_db_val-xfeld = 'X'.
      SELECT DISTINCT * FROM dift_pos_ident INTO TABLE @lt_pos_ident
      FOR ALL ENTRIES IN @gt_deal
      WHERE company_code = @gt_deal-bukrs AND
      deal_number = @gt_deal-rfha AND
      context = 'PCT'.
    ELSE.
      SELECT DISTINCT * FROM dift_pos_ident INTO TABLE @lt_pos_ident
      FOR ALL ENTRIES IN @gt_deal
      WHERE company_code = @gt_deal-bukrs AND
      deal_number = @gt_deal-rfha AND
      valuation_area = @p_val AND "IN <-> = UU 15.03.2019
      context = 'PCT'.
    ENDIF.
  ENDIF.

  SORT lt_pos_ident BY os_guid.
  DELETE ADJACENT DUPLICATES FROM lt_pos_ident.

  IF lt_pos_ident IS NOT INITIAL.
    SELECT * FROM tract_poscontext INTO TABLE @lt_poscontext
    FOR ALL ENTRIES IN @lt_pos_ident
    WHERE pos_identif_oid = @lt_pos_ident-os_guid.
  ENDIF.

  IF lt_poscontext IS NOT INITIAL.
    SELECT * FROM tract_accitem INTO TABLE @lt_accitem
    FOR ALL ENTRIES IN @lt_poscontext
    WHERE document_guid IN @lr_docgu
      AND item_number   IN @lr_itmnu
      AND pc_guid       EQ @lt_poscontext-os_guid
      AND payment_date  EQ @p_keydat.
  ENDIF.

  IF lt_accitem IS NOT INITIAL.
    SELECT mandt,
           os_guid,
           bustransid,
           documentid,
           tr_acc_code,
           valuation_area,
           awref,
           aworg,
           acpostingdate,
           acpostingperiod,
           acpostingyear,
           reversal_awref,
           reversal_aworg,
           acreversaldate,
           acreversalperiod,
           acreversalyear,
           document_state,
           ledgerselection,
           specialledger,
           document_date,
           acc_principle,
           post_cat,
           create_cat,
           ref_bustransid,
           bustranscat,
           deal_number,
           bktxt,
           xblnr,
           acreversalreason,
           tr_rev_reason,
           bupla,
           secco,
           user_ndata_cls,
           user_ndata_oid
       FROM tract_document INTO TABLE @lt_document
    FOR ALL ENTRIES IN @lt_accitem
    WHERE os_guid = @lt_accitem-document_guid AND
    acpostingdate = @p_keydat AND
    document_state IN ('2').
  ENDIF.

  SORT gt_detail BY bukrs rfha.
  SORT lt_document BY os_guid.

  LOOP AT lt_accitem INTO ls_accitem.
    IF  ls_accitem-payment_date <> p_keydat.
      CONTINUE.
    ENDIF.

    CLEAR ls_out.

    READ TABLE lt_document INTO ls_document WITH KEY os_guid = ls_accitem-document_guid BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    READ TABLE gt_deal INTO ls_deal WITH KEY bukrs = ls_document-tr_acc_code
                                             rfha  = ls_document-bktxt(13) BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR: lv_exist, ls_class_acc.
    LOOP AT gt_class_acc INTO ls_class_acc.
      IF ( ls_deal-bukrs CP ls_class_acc-bukrs OR ls_class_acc-bukrs = '' ) AND
      ( ls_deal-val_area CP ls_class_acc-val_area OR ls_class_acc-val_area = '' ) AND
      ( ls_deal-sgsart CP ls_class_acc-sgsart OR ls_class_acc-sgsart = '' ) AND
      ( ls_accitem-dis_flowtype CP ls_class_acc-dis_flowtype OR ls_class_acc-dis_flowtype = '' ) AND
      ( ls_class_acc-sbktyp = '30' ) AND
      ( ls_accitem-gl_account = ls_class_acc-hkont_long ).
        lv_exist = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_exist IS INITIAL.
      CONTINUE.
    ENDIF.

    ls_out-bukrs = ls_deal-bukrs.
    ls_out-rfha = ls_deal-rfha.
    ls_out-rkondgr = ''.
    ls_out-kontrh = ls_deal-kontrh.
    ls_out-rportb = ls_deal-rportb.

    SELECT SINGLE name_org1 INTO @ls_out-kontrh_txt FROM but000
    WHERE partner = @ls_out-kontrh.

    IF gs_db_val-xfeld = 'X'.
      ls_out-val_area = ls_document-valuation_area.
      ls_out-acc_principle = ls_document-acc_principle.
    ENDIF.

    ls_out-sbktyp = gc_sbktyp_val.
    ls_out-keydat = p_keydat.
    ls_out-sgsart = ls_deal-sgsart.
    ls_out-dblfz = ls_deal-dblfz.
    ls_out-delfz = ls_deal-delfz.
    "ls_out-wzbetr = ls_deal-wzbetr.
    ls_out-wzbetr = ls_accitem-position_curr.
    ls_out-dmbtr_val  = ls_accitem-local_amt. "#EC CI_FLDEXT_OK[2610650]
    ls_out-hwaer_val  = ls_accitem-local_curr.
    ls_out-sbktyp_txt = 'Değerleme'(005) ##TEXT_POOL.
    ls_out-bzbetr_cap = ls_deal-bzbetr.
    ls_out-bhwbetr_cap = ls_deal-bhwbetr.
    ls_out-hkont_long = ls_class_acc-hkont_long.
    ls_out-hkont_short = ls_class_acc-hkont_short.
    ls_out-umskz_long = ls_class_acc-umskz_long.
    ls_out-umskz_short = ls_class_acc-umskz_short.

    IF gv_budat_nextyear < ls_out-delfz.
      ls_out-bhwbetr_short = ls_accitem-local_amt * ls_deal-bzbetr_short_current / ( ls_deal-bzbetr_short_current + ls_deal-bzbetr_long ) * -1 .
    ELSE.
      ls_out-bhwbetr_short = ls_accitem-local_amt * -1.
    ENDIF.
    COLLECT  ls_out INTO gt_out.

    ls_detail-bukrs = ls_out-bukrs.
    ls_detail-dmbtr_val = ls_out-dmbtr_val.
    ls_detail-hwaer_val = ls_out-hwaer_val.
    ls_detail-kontrh = ls_out-kontrh.
    ls_detail-rportb = ls_out-rportb.
    ls_detail-sgsart = ls_out-sgsart.
    ls_detail-sbktyp = ls_out-sbktyp.
    ls_detail-rfha = ls_out-rfha.
    ls_detail-dblfz = ls_out-dblfz.
    ls_detail-delfz = ls_out-delfz.
    ls_detail-wzbetr = ls_out-wzbetr.
    ls_detail-dzterm = ls_document-acpostingdate.
    ls_detail-bzbetr = ls_accitem-position_amt.
    ls_detail-bhwbetr = ls_accitem-local_amt.
    ls_detail-dis_flowtype = ls_accitem-dis_flowtype.
    APPEND ls_detail TO gt_detail.

  ENDLOOP.


*  """""""""""""""""Modify posted amounts"""""""""""
  LOOP AT gt_out INTO ls_out.
    IF ls_out-sbktyp <> gc_sbktyp_val.
      CONTINUE.
    ENDIF.

    CLEAR lv_bhwbetr_tot.
    CLEAR lv_bzbetr_tot.

    LOOP AT gt_db_posted INTO ls_posted.
      IF ls_posted-bukrs = ls_out-bukrs AND ls_posted-rfha = ls_out-rfha AND
         ls_posted-sbktyp = gc_sbktyp_val AND ls_posted-val_area = ls_out-val_area AND
         ls_posted-budat  = p_keydat    AND ls_posted-hkont_long = ls_out-hkont_long.

        lv_bzbetr_tot = lv_bzbetr_tot + ls_posted-wrbtr.
        lv_bhwbetr_tot = lv_bhwbetr_tot + ls_posted-dmbtr.
      ENDIF.
    ENDLOOP.

    ls_out-bzbetr_posted = lv_bzbetr_tot.
    ls_out-bhwbetr_posted = lv_bhwbetr_tot.

    ls_out-bzbetr_postable = ls_out-bzbetr_short - ls_out-bzbetr_posted.
    ls_out-bhwbetr_postable = ls_out-bhwbetr_short - ls_out-bhwbetr_posted.
    MODIFY gt_out FROM ls_out.
  ENDLOOP.




ENDFORM. " GET_VALUATION_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_detail USING us_out TYPE gty_out.
  DATA ls_fcat TYPE lvc_s_fcat.
  DATA lt_fcat TYPE lvc_t_fcat.
  DATA ls_detail TYPE gty_flow.



  PERFORM fill_fcat USING ls_fcat 'BUKRS' 'GT_DETAIL_OUT' 'BUKRS' 'BKPF' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'RFHA' 'GT_DETAIL_OUT' 'RFHA' 'VTBFHAPO' '' ##TEXT_POOL.
  ls_fcat-hotspot = 'X'.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'VAL_AREA' 'GT_OUT' 'VALUATION_AREA' 'TRACT_DOCUMENT' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'ACC_PRINCIPLE' 'GT_OUT' 'ACC_PRINCIPLE' 'TRACT_DOCUMENT' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'KONTRH' 'GT_DETAIL_OUT' 'KONTRH' 'VTBFHA' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'RPORTB' 'GT_DETAIL_OUT' 'RPORTB' 'VTBFHA' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'SGSART' 'GT_DETAIL_OUT' 'SGSART' 'VTBFHA' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'SFHAZBA' 'GT_DETAIL_OUT' 'SFHAZBA' 'VTBFHAPO' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'SBKTYP' 'GT_DETAIL_OUT' '' '' 'Kayıt Tipi'(001) ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'DIS_FLOWTYPE' 'GT_DETAIL_OUT' '' '' 'Güncelleme Türü'(002) ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'DZTERM' 'GT_DETAIL_OUT' 'DZTERM' 'VTBFHAPO' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'WZBETR' 'GT_DETAIL_OUT' 'WZBETR' 'VTBFHAPO' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'BZBETR' 'GT_DETAIL_OUT' 'BZBETR' 'VTBFHAPO' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  PERFORM fill_fcat USING ls_fcat 'BHWBETR' 'GT_DETAIL_OUT' 'BHWBETR' 'VTBFHAPO' '' ##TEXT_POOL.
  APPEND ls_fcat TO lt_fcat.

  CLEAR gt_detail_out.

  LOOP AT gt_detail INTO ls_detail.
    IF ls_detail-bukrs = us_out-bukrs AND
       ls_detail-rfha  = us_out-rfha  AND
       ls_detail-sbktyp = us_out-sbktyp.
      APPEND ls_detail TO gt_detail_out.
    ENDIF.
  ENDLOOP.




  PERFORM display_output USING lt_fcat.

ENDFORM. " DISPLAY_DETAIL


*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_TAB  text
*----------------------------------------------------------------------*
FORM display_log_table..
  DATA ls_fcat TYPE lvc_s_fcat.
  DATA lt_fcat TYPE lvc_t_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE    =
      i_structure_name   = 'ZTRM_CT0003'
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_BYPASSING_BUFFER =
      i_internal_tabname = 'GT_CLASS_LOG'
    CHANGING
      ct_fieldcat        = lt_fcat.

  LOOP AT lt_fcat INTO ls_fcat.
    CASE ls_fcat-fieldname.
      WHEN 'BELNR'.
        ls_fcat-hotspot = 'X'.
    ENDCASE.
    MODIFY lt_fcat FROM ls_fcat.
  ENDLOOP.

  PERFORM display_output USING lt_fcat.

ENDFORM. " DISPLAY_LOG_TABLE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_log .


  SELECT mandt,
         rfha,
         bukrs,
         belnr,
         gjahr,
         sbktyp,
         val_area,
         hkont_long,
         rportb,
         keydat,
         budat,
         belnr_rev,
         budat_rev,
         waers,
         wrbtr,
         dmbtr FROM ztrm_ct0003 INTO TABLE @gt_class_log
    WHERE bukrs  IN @s_bukrs AND
          rfha   IN @s_rfha AND
          rportb IN @s_rportb AND
          keydat <= @p_keydat.

  PERFORM display_log_table.


ENDFORM. " DISPLAY_LOG
*&---------------------------------------------------------------------*
*&      Form  MAKE_CARRYFORWARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_carryforward .
  DATA ls_out TYPE gty_out.
  DATA ls_class_acc TYPE ztrm_ct0003.
  DATA lt_class_acc TYPE TABLE OF ztrm_ct0003.
  DATA ls_transfer TYPE ztrm_ct0006.

  LOOP AT gt_out INTO ls_out.
    IF ls_out-check = 'X'.
      ls_class_acc-bukrs = ls_out-bukrs.
      ls_class_acc-belnr = 'A000000000'.
      ls_class_acc-gjahr = p_keydat(4).
      ls_class_acc-keydat = ls_out-keydat.
      ls_class_acc-budat = ls_out-keydat.
      ls_class_acc-sbktyp = ls_out-sbktyp.
      ls_class_acc-rfha = ls_out-rfha.
      ls_class_acc-waers = ls_out-wzbetr.
      ls_class_acc-wrbtr = ls_out-bzbetr_short.
      ls_class_acc-dmbtr = ls_out-bhwbetr_short.
      ls_class_acc-val_area = p_val.
      APPEND ls_class_acc TO lt_class_acc.
    ENDIF.
  ENDLOOP.

  IF lt_class_acc IS NOT INITIAL.
    MODIFY ztrm_ct0003 FROM TABLE lt_class_acc.
    IF sy-subrc IS INITIAL.
      LOOP AT lt_class_acc INTO ls_class_acc.
        ls_transfer-bukrs = ls_class_acc-bukrs.
        ls_transfer-budat = p_keydat.
        MODIFY ztrm_ct0006 FROM ls_transfer.
      ENDLOOP.
    ENDIF.
    MESSAGE i888(sabapdocu) WITH 'Bakiye Devri Tamamlanmıştır.'(027) ##TEXT_POOL.
  ELSE.
    MESSAGE i888(sabapdocu) WITH 'Bakiye Devri sırasında hata oluştu'(028) ##TEXT_POOL.
  ENDIF.

ENDFORM. " MAKE_CARRYFORWARD
*&---------------------------------------------------------------------*
*& Form update_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_log .

  SELECT z1~mandt,
         z1~rfha,
         z1~bukrs,
         z1~belnr,
         z1~gjahr,
         z1~sbktyp,
         z1~val_area,
         z1~hkont_long,
         z1~rportb,
         z1~keydat,
         z1~budat,
         z1~belnr_rev,
         z1~budat_rev,
         z1~waers,
         z1~wrbtr,
         z1~dmbtr,
         z1~inact
   INTO TABLE @DATA(lt_stblg)
   FROM ztrm_ct0003 AS z1
  INNER JOIN bkpf
     ON z1~bukrs  EQ bkpf~bukrs
    AND z1~belnr  EQ bkpf~belnr
    AND z1~gjahr  EQ bkpf~gjahr
    AND bkpf~stblg           NE @space
  WHERE z1~inact  NE @abap_true.
  IF sy-subrc IS INITIAL.
    LOOP AT lt_stblg ASSIGNING FIELD-SYMBOL(<ls_stblg>).
      <ls_stblg>-inact = abap_true.
    ENDLOOP.
    IF sy-subrc IS INITIAL.
      MODIFY ztrm_ct0003 FROM TABLE @lt_stblg.
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ufrs_detay
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM ufrs_detay .
  DATA lv_datum TYPE datum.
  lv_datum = p_keydat.
  lv_datum(4) = lv_datum(4) + 1.
  CLEAR: lv_rfha,lv_tarih, gs_ufrs.
  SORT gt_ufrs BY bukrs rfha dzterm.
  LOOP AT gt_ufrs INTO gs_ufrs.
    IF lv_rfha NE gs_ufrs-rfha.
      APPEND INITIAL LINE TO gt_deep ASSIGNING FIELD-SYMBOL(<lfs_deep>).
      <lfs_deep>-bukrs = gs_ufrs-bukrs.
      <lfs_deep>-rfha = gs_ufrs-rfha.
      lv_rfha = gs_ufrs-rfha.
    ENDIF.
    IF lv_tarih NE gs_ufrs-dzterm.
      APPEND INITIAL LINE TO gt_ufrshesap ASSIGNING FIELD-SYMBOL(<lfs_ufrh>).
      APPEND INITIAL LINE TO <lfs_deep>-payments ASSIGNING FIELD-SYMBOL(<lfs_payments>).
      IF gs_ufrs-dzterm < p_keydat.
        <lfs_ufrh>-kosul = '0'.
      ELSE.
        <lfs_ufrh>-kosul = '1'.
        CLEAR gv_days.
        gv_days = gs_ufrs-dzterm - p_keydat.
        <lfs_ufrh>-days_to_mat =  gv_days.
      ENDIF.
      <lfs_ufrh>-bukrs = gs_ufrs-bukrs.
      <lfs_ufrh>-rfha = gs_ufrs-rfha.
      <lfs_ufrh>-dzterm = gs_ufrs-dzterm.
      <lfs_ufrh>-dblfz  = gs_ufrs-dblfz.
      <lfs_ufrh>-delfz  = gs_ufrs-delfz.
      <lfs_ufrh>-khwkurs = gs_ufrs-khwkurs.
      <lfs_ufrh>-kontrh = gs_ufrs-kontrh.
      <lfs_ufrh>-rfhazu = gs_ufrs-rfhazu.
      <lfs_ufrh>-rkondgr = gs_ufrs-rkondgr.
      <lfs_ufrh>-rportb = gs_ufrs-rportb.
      <lfs_ufrh>-sbktyp = gs_ufrs-sbktyp.
      <lfs_ufrh>-sfhaart = gs_ufrs-sfhaart.
      <lfs_ufrh>-sfhazba = gs_ufrs-sfhazba.
      <lfs_ufrh>-sgsart = gs_ufrs-sgsart.
      <lfs_ufrh>-ssign = gs_ufrs-ssign.
      <lfs_ufrh>-total = <lfs_ufrh>-bzbetr_p + <lfs_ufrh>-bzbetr_i.
      <lfs_payments>-ddispo = gs_ufrs-dzterm.
      <lfs_payments>-scwhr = <lfs_ufrh>-bzbetr_pb.
      <lfs_payments>-bcwhr = <lfs_ufrh>-total.
      lv_tarih = gs_ufrs-dzterm.
    ENDIF.


    IF gs_ufrs-sbktyp = '10'.
      <lfs_ufrh>-bzbetr_p = gs_ufrs-bzbetr * -1 .
      <lfs_ufrh>-bzbetr_pb = gs_ufrs-wzbetr.
      <lfs_ufrh>-sbktyp_p = gs_ufrs-sbktyp.
      <lfs_ufrh>-total = <lfs_ufrh>-bzbetr_p + <lfs_ufrh>-bzbetr_i.
      <lfs_payments>-scwhr = <lfs_ufrh>-bzbetr_pb.
      <lfs_payments>-bcwhr = <lfs_ufrh>-total.
    ELSEIF gs_ufrs-sbktyp BETWEEN '11' AND '14'.
      <lfs_ufrh>-bzbetr_p = gs_ufrs-bzbetr.
      <lfs_ufrh>-bzbetr_pb = gs_ufrs-wzbetr.
      <lfs_ufrh>-sbktyp_p = gs_ufrs-sbktyp.
      <lfs_ufrh>-total = <lfs_ufrh>-bzbetr_p + <lfs_ufrh>-bzbetr_i.
      <lfs_payments>-scwhr = <lfs_ufrh>-bzbetr_pb.
      <lfs_payments>-bcwhr = <lfs_ufrh>-total.

    ELSEIF gs_ufrs-sbktyp = '20'.
      <lfs_ufrh>-bzbetr_i = gs_ufrs-bzbetr.
      <lfs_ufrh>-bzbetr_ib = gs_ufrs-wzbetr.
      <lfs_ufrh>-sbktyp_i = gs_ufrs-sbktyp.
      <lfs_ufrh>-total = <lfs_ufrh>-bzbetr_p + <lfs_ufrh>-bzbetr_i.
      <lfs_payments>-scwhr = <lfs_ufrh>-bzbetr_pb.
      <lfs_payments>-bcwhr = <lfs_ufrh>-total.

    ENDIF.

  ENDLOOP.



  CLEAR lv_rfha.

  LOOP AT gt_ufrshesap ASSIGNING FIELD-SYMBOL(<lfs_ufrshesap>).

    IF lv_rfha NE <lfs_ufrshesap>-rfha.
      CLEAR gv_iff.
      READ TABLE gt_deep INTO DATA(ls_deep) WITH KEY bukrs = <lfs_ufrshesap>-bukrs rfha = <lfs_ufrshesap>-rfha.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'FIMA_IRR_CALCULATE'
          EXPORTING
            i_irrmethod    = 06
            i_szbmeth      = '2'
*           I_SKALID       =
*           I_STARTING_VALUE       = 0
*           I_RUNIT        =
            it_payments    = ls_deep-payments
          IMPORTING
            e_irr          = gv_iff
*           ET_DISP        =
          EXCEPTIONS
            method_invalid = 1
            not_solvable   = 2
            OTHERS         = 3.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
      ENDIF.
      lv_rfha = <lfs_ufrshesap>-rfha.
      <lfs_ufrshesap>-faiz = gv_iff.
    ELSE.
      <lfs_ufrshesap>-faiz = gv_iff.
      IF <lfs_ufrshesap>-kosul = '1'.
        IF <lfs_ufrshesap>-days_to_mat > 0.

          <lfs_ufrshesap>-amortised_cost = <lfs_ufrshesap>-total / ( 1 + gv_iff / 100 ) ** ( <lfs_ufrshesap>-days_to_mat / 365 ).
          <lfs_ufrshesap>-amortised_cost_t = <lfs_ufrshesap>-amortised_cost - <lfs_ufrshesap>-bzbetr_p.

        ENDIF.

      ENDIF.


    ENDIF.
    gs_ufrsson = VALUE #( bukrs = <lfs_ufrshesap>-bukrs
                          rfha = <lfs_ufrshesap>-rfha
                          rfhazu = <lfs_ufrshesap>-rfhazu
                          kontrh = <lfs_ufrshesap>-kontrh
                          rkondgr = <lfs_ufrshesap>-rkondgr
                          dblfz = <lfs_ufrshesap>-dblfz
                          delfz = <lfs_ufrshesap>-delfz
                          khwkurs = <lfs_ufrshesap>-khwkurs
                          sbktyp = '31'
                          rportb = <lfs_ufrshesap>-rportb
                          sfhaart = <lfs_ufrshesap>-sfhaart
                          sgsart = <lfs_ufrshesap>-sgsart
                          wzbetrt = <lfs_ufrshesap>-bzbetr_pb
                          t_total = COND #( WHEN <lfs_ufrshesap>-dzterm LE lv_datum THEN <lfs_ufrshesap>-amortised_cost_t
                                            ELSE 0 )
                          total_amortised = <lfs_ufrshesap>-amortised_cost_t
                          ).
    IF gs_ufrsson-total_amortised IS NOT INITIAL.
      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
        EXPORTING
          date             = p_keydat
          foreign_amount   = gs_ufrsson-total_amortised
          foreign_currency = gs_ufrsson-wzbetrt
          local_currency   = gs_t001-waers
        IMPORTING
          local_amount     = gs_ufrsson-total_amortised_up.
      .
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.
    IF gs_ufrsson-t_total IS NOT INITIAL.

    ENDIF.
    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
        date             = p_keydat
        foreign_amount   = gs_ufrsson-t_total
        foreign_currency = gs_ufrsson-wzbetrt
        local_currency   = gs_t001-waers
      IMPORTING
        local_amount     = gs_ufrsson-t_totalup.
    .
    IF sy-subrc <> 0.
    ENDIF.
    .  COLLECT gs_ufrsson INTO gt_ufrsson.

    IF <lfs_ufrshesap>-dzterm LE lv_datum AND gs_ufrsson-t_total NE 0.
      CLEAR gs_detail.
      gs_detail = VALUE #(  bukrs = gs_ufrsson-bukrs
                            rfha = gs_ufrsson-rfha
                            rfhazu = gs_ufrsson-rfhazu
                            delfz = gs_ufrsson-delfz
                            dblfz = gs_ufrsson-dblfz
                            dzterm = <lfs_ufrshesap>-dzterm
                            kontrh = gs_ufrsson-kontrh
                            sbktyp = gs_ufrsson-sbktyp
                            sfhaart = gs_ufrsson-sfhaart
                            sgsart = gs_ufrsson-sgsart
                            sfhazba = gs_ufrsson-sfhazba
                            bzbetr = gs_ufrsson-t_total
                           wzbetr = gs_ufrsson-wzbetrt

                          ).
      APPEND gs_detail TO gt_detail.
    ENDIF.

  ENDLOOP.


ENDFORM.
