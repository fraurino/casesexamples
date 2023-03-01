procedimento para tratar o prefixo e removê-lo do arquivo xml lido.
procedure RemovePrefixFromXMLFile(const inputFileName, outputFileName, prefix: string);
  var
    inputFile, outputFile: TextFile;
    line: string;
  begin
    AssignFile(inputFile, inputFileName);
    AssignFile(outputFile, outputFileName);
    Reset(inputFile);
    Rewrite(outputFile);
    while not EOF(inputFile) do
    begin
      ReadLn(inputFile, line);
      line := StringReplace(line, prefix , '', [rfReplaceAll]);
      WriteLn(outputFile, line);
    end;
    CloseFile(inputFile);
    CloseFile(outputFile);
  end;
  
  exemplo de uso;
  
  var
   inputFileName, outputFileName: string;
   n : integer;
   begin
    OpenDialog1.FileName  :=  '';
    OpenDialog1.Title := 'Selecione a NFe';
    OpenDialog1.DefaultExt := '*-nfe.XML';
    OpenDialog1.Filter := 'Arquivos NFe (*-nfe.XML)|*-nfe.XML|Arquivos XML (*.XML)|*.XML|Arquivos TXT (*.TXT)|*.TXT|Todos os Arquivos (*.*)|*.*';

    OpenDialog1.InitialDir := ACBrNFe1.Configuracoes.Arquivos.PathSalvar;
    
    if OpenDialog1.Execute then
  begin
    ACBrNFe1.NotasFiscais.Clear;
    //tenta TXT
    ACBrNFe1.NotasFiscais.Add;
    NFeRTXT := TNFeRTXT.Create(ACBrNFe1.NotasFiscais.Items[0].NFe);
    NFeRTXT.CarregarArquivo(OpenDialog1.FileName);
    if NFeRTXT.LerTxt then
       NFeRTXT.Free
    else
    begin
       NFeRTXT.Free;
       //tenta XML
       ACBrNFe1.NotasFiscais.Clear;
       try
          inputFileName  := OpenDialog1.FileName ;//arquivo xml original
          outputFileName := ExtractFilePath(Opendialog1.FileName) +  'new '+ExtractFileName(OpenDialog1.FileName) ;//arquivo xml sem prefixo;
          RemovePrefixFromXMLFile(inputFileName, outputFileName, 'ns0:'); // executando o procedumento de tratamento;
          ACBrNFe1.NotasFiscais.LoadFromFile(outputFileName ); //carregando o novo xml tratado sem prefixo;
          
          //lendo dados;
          trvwDocumento.items.Clear;//add componente TTreeView;
          for n:=0 to ACBrNFe1.NotasFiscais.Count-1 do
          begin
          with ACBrNFe1.NotasFiscais.Items[n].NFe do
           begin

             Nota := trvwDocumento.Items.Add(nil,infNFe.ID);
             trvwDocumento.Items.AddChild(Nota,'ID= ' +infNFe.ID);
             Node := trvwDocumento.Items.AddChild(Nota,'procNFe');
             trvwDocumento.Items.AddChild(Node,'tpAmb= '     +TpAmbToStr(procNFe.tpAmb));
             trvwDocumento.Items.AddChild(Node,'verAplic= '  +procNFe.verAplic);
             trvwDocumento.Items.AddChild(Node,'chNFe= '     +procNFe.chNFe);
             trvwDocumento.Items.AddChild(Node,'dhRecbto= '  +DateTimeToStr(procNFe.dhRecbto));
             trvwDocumento.Items.AddChild(Node,'nProt= '     +procNFe.nProt);
             trvwDocumento.Items.AddChild(Node,'digVal= '    +procNFe.digVal);
             trvwDocumento.Items.AddChild(Node,'cStat= '     +IntToStr(procNFe.cStat));
             trvwDocumento.Items.AddChild(Node,'xMotivo= '   +procNFe.xMotivo);
           end;
          end;
       except
          ShowMessage('Arquivo NFe Inválido');
          exit;
       end;
    end;
    
   end;
