'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 21 April 2012 at 9:26:57 pm'!
'Description Please enter a description for this package '!
!classDefinition: #PasswordGenerator category: #PasswordGenerator!
ActiveModel subclass: #PasswordGenerator
	instanceVariableNames: 'newPassword lowerCase upperCase digits specialChars passwordLength'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PasswordGenerator'!
!classDefinition: 'PasswordGenerator class' category: #PasswordGenerator!
PasswordGenerator class
	instanceVariableNames: ''!


!PasswordGenerator commentStamp: 'gsa 4/21/2012 18:45' prior: 0!
PasswordGenerator passwordLength: 8. 
'abcdefghijklmnopqrstuvwxyz' atRandom.!

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:20'!
digits
^ digits! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 4/21/2012 21:21'!
digits: aString
	digits _ aString! !

!PasswordGenerator methodsFor: 'actions' stamp: 'gsa 4/21/2012 19:59'!
generatPassword

passwordLength = 8 ifTrue: [self generatePasswordWithLength8].! !

!PasswordGenerator methodsFor: 'actions' stamp: 'gsa 4/21/2012 21:26'!
generatePassword

passwordLength = 8 ifTrue: [self generatePasswordWithLength8].! !

!PasswordGenerator methodsFor: 'actions' stamp: 'gsa 4/21/2012 20:00'!
generatePasswordWithLength8
	| index tmpPassword |
	tmpPassword _ String new: 8.
	index _ 0.
	[ index < 8 ] whileTrue: [
		index _ index + 1.
		tmpPassword
			at: index
			put: 'abcdefghijklmnopqrstuvwxyz' atRandom ].
	"	
	Transcript
		 show: tmpPassword;
		 cr."
	self newPassword: tmpPassword.! !

!PasswordGenerator methodsFor: 'initialize-release' stamp: 'gsa 4/21/2012 21:26'!
initPasswordGeneratorWithLength: anInteger
	self initializeData.
	self passwordLength: anInteger.
	self generatePassword.! !

!PasswordGenerator methodsFor: 'initialize-release' stamp: 'gsa 4/21/2012 21:25'!
initializeData
	self lowerCase: 'abcdefghijklmnopqrstuvwxyz'.
	self upperCase: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
	self digits: '0123456789'.
	self specialChars: '!!¡¿?@$&'.! !

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

!PasswordGenerator class methodsFor: 'instance creation' stamp: 'gsa 4/21/2012 20:22'!
passwordLength: anInteger 

	^ self new initPasswordGeneratorWithLength: anInteger ! !
