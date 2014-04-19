pro mail,body,email,subject = subject, silent=silent
; A stupid wrapper to use the LINUX program MAIL from IDL w/ SPAWN

; !email is defined in my IDL_STARTUP.PRO file using this command:
;        defsysv,'!email','name@email.edu'


if not keyword_set(subject) then subject = 'IDLautomailer'

if not keyword_set(email) then begin
    defsysv,'!email',exists=exists
    if not exists then begin
        if not keyword_set(silent) then print,'No Email provided'
        return
    endif
    if exists then begin 
        email = !email
        if not keyword_set(silent) then print,'Mail: Using email=',!email
    endif
endif 

spawn,'echo "'+body+'" | mail -s "'+subject+'" '+email

return
end
