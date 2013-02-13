'From Cuis 4.1 of 12 December 2012 [latest update: #1576] on 13 February 2013 at 6:24:43 pm'!
'Description Please enter a description for this package '!
!classDefinition: #PasswordGeneratorModel category: #'PasswordGenerator-Model'!
ActiveModel subclass: #PasswordGeneratorModel
	instanceVariableNames: 'newPassword lowerCase upperCase digits specialChars passwordLength bolLowerCase bolUpperCase bolDigits bolSpecialChars charset'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PasswordGenerator-Model'!
!classDefinition: 'PasswordGeneratorModel class' category: #'PasswordGenerator-Model'!
PasswordGeneratorModel class
	instanceVariableNames: ''!

!classDefinition: #PasswordGeneratorModelTests category: #'PasswordGenerator-Model-Tests'!
TestCase subclass: #PasswordGeneratorModelTests
	instanceVariableNames: 'myPasswordGenerator'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PasswordGenerator-Model-Tests'!
!classDefinition: 'PasswordGeneratorModelTests class' category: #'PasswordGenerator-Model-Tests'!
PasswordGeneratorModelTests class
	instanceVariableNames: ''!


!PasswordGeneratorModel commentStamp: 'gsa 1/10/2013 07:50' prior: 0!
To instantiate the class:

   myPasswordGenerator := PasswordGeneratorModel passwordLength: 9 lowerCase: true upperCase: true digits: true specialChars: true.


To generate the password:

	myPasswordGenerator generatePassword.!

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:51'!
bolDigits
^ bolDigits! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:52'!
bolDigits: aBoolean
	bolDigits _ aBoolean! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:52'!
bolLowerCase
^ bolLowerCase! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:53'!
bolLowerCase: aBoolean
	bolLowerCase _ aBoolean! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:53'!
bolSpecialChars
^ bolSpecialChars! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:54'!
bolSpecialChars: aBoolean
	bolSpecialChars _ aBoolean! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:54'!
bolUpperCase
^ bolUpperCase! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:55'!
bolUpperCase: aBoolean
	bolUpperCase _ aBoolean! !

!PasswordGeneratorModel methodsFor: 'actions' stamp: 'gsa 1/9/2013 21:43'!
buildCharset
	
	
	bolLowerCase ifTrue: [charset := lowerCase]
					ifFalse: [charset := ''].
	
	
	bolUpperCase ifTrue: [charset := charset,upperCase].
	
	bolDigits ifTrue: [charset := charset,digits].
					
	bolSpecialChars ifTrue: [charset := charset,specialChars].
	
	
	"If no option is selected, then ALL is used by default"
	charset = '' ifTrue:[charset := lowerCase,upperCase,digits,specialChars]! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:12'!
charset
^ charset! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 1/9/2013 19:12'!
charset: aString
	charset _ aString! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:20'!
digits
^ digits! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:21'!
digits: aString
	digits _ aString! !

!PasswordGeneratorModel methodsFor: 'actions' stamp: 'gsa 1/9/2013 20:23'!
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

!PasswordGeneratorModel methodsFor: 'initialize-release' stamp: 'gsa 1/10/2013 08:44'!
initiPasswordGenerator: anInteger lowerCase: aLowerCase upperCase: anUpperCase digits: aDigits specialChars: aSpecialChars.

	self passwordLength: anInteger.
	
	self bolLowerCase: aLowerCase.
	self bolUpperCase: anUpperCase.
	self bolDigits: aDigits.
	self bolSpecialChars: aSpecialChars.
	
	self initializeData.
	
	! !

!PasswordGeneratorModel methodsFor: 'initialize-release' stamp: 'gsa 1/9/2013 19:40'!
initializeData


	self lowerCase: 'abcdefghijklmnopqrstuvwxyz'.
	self upperCase: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
	self digits: '0123456789'.
	self specialChars: '!!¡¿?@$&'.! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 1/9/2013 20:20'!
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

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:18'!
lowerCase
^ lowerCase! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:19'!
lowerCase: aString
	lowerCase _ aString! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 4/21/2012 20:21'!
newPassword
^ newPassword! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 4/21/2012 20:22'!
newPassword: aNewPassword
	newPassword _ aNewPassword! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 4/21/2012 20:22'!
passwordLength
	^ passwordLength.! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 1/10/2013 08:45'!
passwordLength: anInteger

	" PasswordGenerator generates passwords between 8 and 100 chars"

	anInteger < 8 ifTrue: [passwordLength := 8] ifFalse: [anInteger > 100 ifTrue: [passwordLength := 100] ifFalse: [passwordLength := anInteger]].

! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:22'!
specialChars
^ specialChars! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:21'!
specialChars: aString
	specialChars _ aString! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:20'!
upperCase
^ upperCase! !

!PasswordGeneratorModel methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:19'!
upperCase: aString
	upperCase _ aString! !

!PasswordGeneratorModel class methodsFor: 'instance creation' stamp: 'gsa 1/10/2013 08:41'!
passwordLength: anInteger lowerCase: aLowerCase upperCase: anUpperCase digits: aDigits specialChars: aSpecialChars

	^ self new initiPasswordGenerator: anInteger lowerCase: aLowerCase upperCase: anUpperCase digits: aDigits specialChars: aSpecialChars.! !

!PasswordGeneratorModelTests methodsFor: 'initialize-release' stamp: 'gsa 1/10/2013 08:35'!
setUp

 myPasswordGenerator := PasswordGeneratorModel passwordLength: 9 lowerCase: true upperCase: true digits: true specialChars: true.! !

!PasswordGeneratorModelTests methodsFor: 'initialize-release' stamp: 'gsa 1/10/2013 07:49'!
tearDown

 myPasswordGenerator := nil! !

!PasswordGeneratorModelTests methodsFor: 'testing' stamp: 'gsa 2/13/2013 18:24'!
testNewIncludeDigits
	| myArray |

	myPasswordGenerator bolLowerCase: false;
							bolUpperCase: false;
							bolDigits: true;
							bolSpecialChars: false.

	myPasswordGenerator generatePassword.

	myArray := myPasswordGenerator digits asArray.

	myPasswordGenerator newPassword do: [:each | self assert: [myArray includes: each]].! !

!PasswordGeneratorModelTests methodsFor: 'testing' stamp: 'gsa 1/10/2013 09:28'!
testNewLowerCaseOnly
	| myArray |

	myPasswordGenerator bolLowerCase: true;
							bolUpperCase: false;
							bolDigits: false;
							bolSpecialChars: false.

	myPasswordGenerator generatePassword.

	myArray := myPasswordGenerator lowerCase asArray.

	myPasswordGenerator newPassword do: [:each | self assert: [myArray includes: each]].! !

!PasswordGeneratorModelTests methodsFor: 'testing' stamp: 'gsa 1/10/2013 08:39'!
testNewPasswordSize0

	myPasswordGenerator passwordLength: 7.
	myPasswordGenerator generatePassword.
	self assert: [ myPasswordGenerator newPassword size = 8].! !

!PasswordGeneratorModelTests methodsFor: 'testing' stamp: 'gsa 1/10/2013 08:46'!
testNewPasswordSize120

	myPasswordGenerator passwordLength: 120.
	myPasswordGenerator generatePassword.
	self assert: [ myPasswordGenerator newPassword size = 100].! !

!PasswordGeneratorModelTests methodsFor: 'testing' stamp: 'gsa 1/10/2013 08:46'!
testNewPasswordSize20

	myPasswordGenerator passwordLength: 20.
	myPasswordGenerator generatePassword.
	self assert: [ myPasswordGenerator newPassword size = 20].! !

!PasswordGeneratorModelTests methodsFor: 'testing' stamp: 'gsa 1/10/2013 08:38'!
testNewPasswordSize9

	myPasswordGenerator generatePassword.
	self assert: [ myPasswordGenerator newPassword size = 9].! !

!PasswordGeneratorModelTests methodsFor: 'testing' stamp: 'gsa 1/10/2013 09:28'!
testNewUpperCaseOnly
	| myArray |

	myPasswordGenerator bolLowerCase: false;
							bolUpperCase: true;
							bolDigits: false;
							bolSpecialChars: false.

	myPasswordGenerator generatePassword.

	myArray := myPasswordGenerator upperCase asArray.

	myPasswordGenerator newPassword do: [:each | self assert: [myArray includes: each]].! !
