'From Cuis 4.1 of 12 December 2012 [latest update: #1549] on 9 January 2013 at 9:30:13 pm'!
'Description Please enter a description for this package '!
!classDefinition: #PasswordGenerator category: #PasswordGenerator!
ActiveModel subclass: #PasswordGenerator
	instanceVariableNames: 'newPassword lowerCase upperCase digits specialChars passwordLength bolLowerCase bolUpperCase bolDigits bolSpecialChars charset'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PasswordGenerator'!
!classDefinition: 'PasswordGenerator class' category: #PasswordGenerator!
PasswordGenerator class
	instanceVariableNames: ''!


!PasswordGenerator commentStamp: 'gsa 1/9/2013 20:14' prior: 0!
To instantiate the class:

   myPwGenerator := PasswordGenerator passwordLength: 9 lowerCase: true upperCase: true digits: true specialChars: true.


To generate the password:

	myPwGenerator generatePassword.!

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:51'!
bolDigits
^ bolDigits! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:52'!
bolDigits: aBoolean
	bolDigits _ aBoolean! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:52'!
bolLowerCase
^ bolLowerCase! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:53'!
bolLowerCase: aBoolean
	bolLowerCase _ aBoolean! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:53'!
bolSpecialChars
^ bolSpecialChars! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:54'!
bolSpecialChars: aBoolean
	bolSpecialChars _ aBoolean! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:54'!
bolUpperCase
^ bolUpperCase! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:55'!
bolUpperCase: aBoolean
	bolUpperCase _ aBoolean! !

!PasswordGenerator methodsFor: 'actions' stamp: 'gsa 1/9/2013 21:29'!
buildCharset

	"
	self lowerCase: 'abcdefghijklmnopqrstuvwxyz'.
	self upperCase: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
	self digits: '0123456789'.
	self specialChars: '!!¡¿?@$&'.
	"	
	
	
	bolLowerCase ifTrue: [
		bolUpperCase ifTrue: [
			bolDigits ifTrue: [
				bolSpecialChars ifTrue: [charset := lowerCase,upperCase,digits,specialChars]
									ifFalse: [charset := lowerCase,upperCase,digits]]
								ifFalse:[
									
									
									bolSpecialChars ifTrue: [charset := lowerCase,upperCase,specialChars]
									ifFalse: [charset := lowerCase,upperCase]]
							
							
								
									]
									]! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:12'!
charset
^ charset! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:12'!
charset: aString
	charset _ aString! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:20'!
digits
^ digits! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:21'!
digits: aString
	digits _ aString! !

!PasswordGenerator methodsFor: 'actions' stamp: 'gsa 1/9/2013 20:23'!
generatePassword
	| index tmpPassword |
	
	self buildCharset.
	
	tmpPassword _ String new: self passwordLength.
	
	
	index _ 0.
	[ index < passwordLength ] whileTrue: [
		index _ index + 1.
		tmpPassword
			at: index
			"put: 'abcdefghijklmnopqrstuvwxyz' atRandom" 
			put: charset atRandom].
"	
	Transcript
		 show: tmpPassword;
		 newLine.
	"
	self newPassword: tmpPassword.
	
	^ self newPassword.! !

!PasswordGenerator methodsFor: 'initialize-release' stamp: 'gsa 1/9/2013 20:30'!
initiPasswordGenerator: aLength lowerCase: aLowerCase upperCase: anUpperCase digits: aDigits specialChars: aSpecialChars.

	" PasswordGenerator generates passwords between 8 and 100 chars"
	aLength < 8 ifTrue: [self passwordLength: 8] ifFalse: [aLength > 100 ifTrue: [self passwordLength: 100] ifFalse: [self passwordLength: aLength]].
	
	self bolLowerCase: aLowerCase.
	self bolUpperCase: anUpperCase.
	self bolDigits: aDigits.
	self bolSpecialChars: aSpecialChars.
	
	self initializeData.
	
	! !

!PasswordGenerator methodsFor: 'initialize-release' stamp: 'gsa 1/9/2013 19:40'!
initializeData


	self lowerCase: 'abcdefghijklmnopqrstuvwxyz'.
	self upperCase: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
	self digits: '0123456789'.
	self specialChars: '!!¡¿?@$&'.! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 1/9/2013 20:20'!
license
^ 'Copyright (c) 2012 Germán S. Arduino (gsa @ arsol.net)

Permission is hereby granted, free of charge, to any
person obtaining a copy of this software and associated
documentation files (the "Software"), to deal in the
Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the
Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice
shall be included in all copies or substantial portions of
the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.'! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:18'!
lowerCase
^ lowerCase! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:19'!
lowerCase: aString
	lowerCase _ aString! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 4/21/2012 20:21'!
newPassword
^ newPassword! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 4/21/2012 20:22'!
newPassword: aNewPassword
	newPassword _ aNewPassword! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 4/21/2012 20:22'!
passwordLength
	^ passwordLength.! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 4/21/2012 20:22'!
passwordLength: anInteger
	passwordLength := anInteger.! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:22'!
specialChars
^ specialChars! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:21'!
specialChars: aString
	specialChars _ aString! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:20'!
upperCase
^ upperCase! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:19'!
upperCase: aString
	upperCase _ aString! !

!PasswordGenerator class methodsFor: 'instance creation' stamp: 'gsa 1/9/2013 19:42'!
passwordLength: aLength lowerCase: aLowerCase upperCase: anUpperCase digits: aDigits specialChars: aSpecialChars

	^ self new initiPasswordGenerator: aLength lowerCase: aLowerCase upperCase: anUpperCase digits: aDigits specialChars: aSpecialChars.! !
