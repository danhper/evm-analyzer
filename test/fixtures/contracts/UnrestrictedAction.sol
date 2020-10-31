pragma solidity >=0.4.25 <0.7.0;

contract UnrestrictedAction {
    address payable constant theVoid = 0x0000000000000000000000000000000000000000;
    address owner;
    mapping(address => uint256) public balances;

    constructor() public {
        owner = msg.sender;
    }

    function() external payable {}

    function unsafeSetOwner() public {
        owner = msg.sender;
    }

    function unsafeCall(uint256 amount) public {
        theVoid.send(amount);
    }

    function unsafeCallData(address payable addr, uint256 amount) public {
        addr.send(amount);
    }

    function unsafeDelegate(address addr) public {
        addr.delegatecall(abi.encodeWithSignature("sendToNull(uint)", 1));
    }

    function safeCallSender(uint256 amount) public {
        msg.sender.send(amount);
    }

    function increaseBalance() public payable {
        balances[msg.sender] += msg.value;
    }

    function safeSetOwner(address newOwner) public {
        if (msg.sender == owner) {
            owner = newOwner;
        }
    }

    function unsafeSelfdestruct() public {
        selfdestruct(theVoid);
    }

    function safeSelfdestruct() public {
        if (msg.sender == owner) {
            selfdestruct(theVoid);
        }
    }
}
