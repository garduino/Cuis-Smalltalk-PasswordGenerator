'From Cuis 4.0 of 21 April 2012 [latest update: #1260] on 21 April 2012 at 7:32:14 pm'!
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

!PasswordGenerator methodsFor: 'actions' stamp: 'gsa 3/25/2012 14:57'!
generatePassword		passwordLength = 8 ifTrue: [self generatePasswordWithLength8].	! !

!PasswordGenerator methodsFor: 'actions' stamp: 'gsa 3/25/2012 15:50'!
generatePasswordWithLength8	| index tmpPassword |	tmpPassword _ String new: 8.	index _ 0.	[ index < 8 ] whileTrue: [		index _ index + 1.		tmpPassword			at: index			put: 'abcdefghijklmnopqrstuvwxyz' atRandom ].	"		Transcript		 show: tmpPassword;		 cr."	self newPassword: tmpPassword.	! !

!PasswordGenerator methodsFor: 'initialize-release' stamp: 'gsa 3/25/2012 14:52'!
initPasswordGeneratorWithLength: anInteger	self passwordLength: anInteger.	self generatePassword.! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 3/25/2012 14:36'!
newPassword^ newPassword! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 3/25/2012 15:48'!
newPassword: aNewPassword	newPassword _ aNewPassword! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 3/25/2012 14:49'!
passwordLength	^ passwordLength.! !

!PasswordGenerator methodsFor: 'accessing' stamp: 'gsa 3/25/2012 14:50'!
passwordLength: anInteger	passwordLength := anInteger.! !

!PasswordGenerator class methodsFor: 'instance creation' stamp: 'gsa 3/25/2012 15:08'!
passwordLength: anInteger 	^ self new initPasswordGeneratorWithLength: anInteger ! !
