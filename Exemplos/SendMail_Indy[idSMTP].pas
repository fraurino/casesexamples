uses
IdMessage, IdUserPassProvider, IdSASL, IdSASLUserPass, IdSASLLogin,
IdTCPConnection, IdTCPClient, IdExplicitTLSClientServerBase, IdMessageClient,
IdSMTPBase, IdSMTP, IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL,
IdSSLOpenSSL, IdBaseComponent, IdComponent, IdServerIOHandler;

example

 var
    idSMTP1: TIdSMTP;
    idSASLLogin: TIdSASLLogin;
    idUserPassProvider: TIdUserPassProvider;
    email: TIdMessage;
    port : integer;
  begin
    idSMTP1 := TIdSMTP.Create(nil);
    try
      idSMTP1.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(idSMTP1);
      idSMTP1.UseTLS := utUseExplicitTLS;
      TIdSSLIOHandlerSocketOpenSSL(idSMTP1.IOHandler).SSLOptions.Method := sslvTLSv1_2;
      TIdSSLIOHandlerSocketOpenSSL(idSMTP1.IOHandler).SSLOptions.SSLVersions := [sslvTLSv1_2];


      idSMTP1.Host := edtHost.text;
      port := 587;
      idSMTP1.Port := port;

      idSASLLogin := TIdSASLLogin.Create(idSMTP1);
      idUserPassProvider := TIdUserPassProvider.Create(idSASLLogin);

      idSASLLogin.UserPassProvider := idUserPassProvider;
      idUserPassProvider.Username  := edtUser.Text;
      idUserPassProvider.Password  := edtPassword.text;

      idSMTP1.AuthType := satSASL;
      idSMTP1.SASLMechanisms.Add.SASL := idSASLLogin;
      try
        idSMTP1.Connect;
        try
          idSMTP1.Authenticate;
          IdMessage1.MessageParts.Clear;
          IdMessage1.From.Address:= 'nao responder';
          IdMessage1.Subject:= edSubject.Text +'|Indy';
          IdMessage1.Body.Text := mAltBody.text;
          IdMessage1.Recipients.EMailAddresses  := edtAddressEmail.Text;
          IdSMTP1.Send(IdMessage1);
        finally
          idSMTP1.Disconnect;
        end;
        ShowMessage('OK - Mail Indy[idSMTP]');
      except
        on E: Exception do
        begin
          ShowMessage(Format('error Mail!'#13'[%s] %s', [E.ClassName, E.Message]));
          raise;
        end;
      end;
    finally
      idSMTP1.Free;
    end;
  end;
